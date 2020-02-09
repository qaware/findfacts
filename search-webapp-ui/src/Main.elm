module Main exposing (..)

import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Bootstrap.Navbar as Navbar
import Browser
import Browser.Navigation as Navigation
import Debug exposing (log)
import Entities exposing (ResultShortlist)
import Html exposing (Html, br, div, h1, text)
import Html.Attributes exposing (href)
import Http exposing (Error(..), expectJson, jsonBody)
import List
import PagingComponent as Paging
import Query exposing (AbstractFQ(..), FacetQuery, FacetResult, Field(..), FilterQuery, FilterTerm(..), decode, encodeFacetQuery, encodeFilterQuery)
import ResultsComponent as Results
import SearchComponent as Search
import Url exposing (Url)
import Url.Parser as UrlParser exposing ((</>), (<?>))



-- APPLICATION


{-| Main application entry point
-}
main : Program () Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlChange = UrlChanged
        , onUrlRequest = LinkClicked
        }


{-| Application initializer
-}
init : () -> Url -> Navigation.Key -> ( Model, Cmd Msg )
init _ url key =
    let
        baseUrl =
            Url.toString { url | path = "", query = Nothing, fragment = Nothing }

        ( navState, navCmd ) =
            Navbar.initialState NavbarMsg

        ( model, urlCmd ) =
            urlUpdate url (Model baseUrl key navState (Home Search.init Paging.init Results.empty))
    in
    ( model, Cmd.batch [ urlCmd, navCmd ] )



-- MODEL


{-| Model containing general state of the application
-}
type alias Model =
    { apiBaseUrl : String
    , navKey : Navigation.Key
    , navState : Navbar.State
    , page : Page
    }


{-| Page to use in model, stores state
-}
type Page
    = Home Search.State Paging.State Results.State
    | Example Results.State
    | Syntax
    | Imprint
    | NotFound



-- UPDATE


type Msg
    = Batch (List Msg)
      -- Routing
    | LinkClicked Browser.UrlRequest
    | UrlChanged Url
    | NavbarMsg Navbar.State
      -- Executing requests
    | ExecuteQuery FacetQuery Int
    | QueryChanged String
    | FilterResult (Result Http.Error ResultShortlist)
    | FacetResult Int (Result Http.Error FacetResult)
      -- s
    | SearchMsg Search.State
    | PagingMsg Paging.State
    | ResultsMsg Results.State


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Batch [] ->
            ( model, Cmd.none )

        Batch (m :: ms) ->
            let
                ( model1, cmd1 ) =
                    update m model

                ( model2, cmd2 ) =
                    update (Batch ms) model1
            in
            ( model2, Cmd.batch [ cmd1, cmd2 ] )

        LinkClicked urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( model, Navigation.pushUrl model.navKey (Url.toString url) )

                Browser.External href ->
                    ( model, Navigation.load href )

        UrlChanged url ->
            urlUpdate url model

        NavbarMsg state ->
            ( { model | navState = state }, Cmd.none )

        ExecuteQuery facetQuery id ->
            ( model, executeFacetQuery model.apiBaseUrl facetQuery id )

        QueryChanged encodedSearcherState ->
            ( model, Navigation.pushUrl model.navKey encodedSearcherState )

        _ ->
            case model.page of
                Home search paging results ->
                    let
                        ( page, cmd ) =
                            updateHome msg search paging results
                    in
                    ( { model | page = page }, cmd )

                _ ->
                    ( model, Cmd.none )


{-| Update main home page. Ignores other messages.
-}
updateHome : Msg -> Search.State -> Paging.State -> Results.State -> ( Page, Cmd Msg )
updateHome msg search paging results =
    case msg of
        FilterResult result ->
            case result of
                Ok res ->
                    ( Ok res |> Results.init |> Home search Paging.init, Cmd.none )

                Err e ->
                    let
                        cause =
                            case e of
                                BadUrl _ ->
                                    "Invalid backend configuration"

                                Timeout ->
                                    "Backend timed out"

                                NetworkError ->
                                    "Could not reach server"

                                BadStatus status ->
                                    "Server error: " ++ String.fromInt status

                                BadBody body ->
                                    "Could not read response" ++ body
                    in
                    ( Home { search | lastQuery = Nothing } paging (Results.init (Err cause)), Cmd.none )

        FacetResult id result ->
            case result of
                Ok res ->
                    ( Home (Search.update search res id) paging results, Cmd.none )

                Err e ->
                    -- TODO improve facet error strategy
                    let
                        _ =
                            log "Error in facet"
                    in
                    ( Home search paging results, Cmd.none )

        SearchMsg state ->
            ( Home state paging results, Cmd.none )

        PagingMsg state ->
            ( Home search state results, Cmd.none )

        ResultsMsg state ->
            ( Home search paging state, Cmd.none )

        _ ->
            ( Home search paging results, Cmd.none )


{-| Parses url and updates model accordingly.
-}
urlUpdate : Url -> Model -> ( Model, Cmd Msg )
urlUpdate url model =
    case UrlParser.parse routeParser url of
        Nothing ->
            ( { model | page = NotFound }
            , Cmd.none
            )

        Just page ->
            case ( model.page, page ) of
                ( Home oldSearch oldPaging oldResults, Home search paging results ) ->
                    let
                        fq =
                            Search.buildFQ search
                    in
                    if oldSearch.lastQuery == Just fq && oldPaging == paging then
                        ( { model | page = Home { search | lastQuery = Just fq } paging oldResults }, Cmd.none )

                    else
                        ( { model | page = Home { search | lastQuery = Just fq } paging Results.Searching }, executeFilterQuery model.apiBaseUrl (Paging.buildFilterQuery fq paging) )

                ( _, Home search paging results ) ->
                    let
                        fq =
                            Search.buildFQ search

                        filterQuery =
                            Paging.buildFilterQuery fq paging
                    in
                    ( { model | page = Home { search | lastQuery = Just fq } paging Results.Searching }, executeFilterQuery model.apiBaseUrl filterQuery )

                ( _, Example results ) ->
                    ( { model | page = Example Results.Searching }, executeFilterQuery model.apiBaseUrl (FilterQuery (Query.Filter [ ( Query.Name, Query.Term "*gauss*" ) ]) 10 Nothing) )

                _ ->
                    ( { model | page = page }, Cmd.none )


routeParser : UrlParser.Parser (Page -> a) a
routeParser =
    UrlParser.oneOf
        [ UrlParser.map (Home Search.init Paging.init Results.empty) UrlParser.top

        -- TODO
        --, UrlParser.map (\search page -> Home search page Results.empty) (UrlParser.s "search" <?> Search.urlParser UrlParser.fragment Paging.cursorParser)
        , UrlParser.map (Example Results.empty) (UrlParser.s "example")
        , UrlParser.map Syntax (UrlParser.s "syntax")
        , UrlParser.map Imprint (UrlParser.s "imprint")
        ]


executeFacetQuery : String -> FacetQuery -> Int -> Cmd Msg
executeFacetQuery apiBaseUrl query id =
    Http.post
        { url = apiBaseUrl ++ "/v1/facet"
        , body = jsonBody (encodeFacetQuery query)
        , expect = expectJson (FacetResult id) decode
        }


executeFilterQuery : String -> FilterQuery -> Cmd Msg
executeFilterQuery apiBaseUrl query =
    Http.post
        { url = apiBaseUrl ++ "/v1/search"
        , body = jsonBody (encodeFilterQuery query)
        , expect = expectJson FilterResult Entities.decoder
        }



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        (Navbar.subscriptions model.navState NavbarMsg
            :: (case model.page of
                    Home searcher _ _ ->
                        [ Search.subscriptions searcher SearchMsg ]

                    _ ->
                        []
               )
        )



-- VIEW


view : Model -> Browser.Document Msg
view model =
    { title = "FindFacts"
    , body =
        [ div []
            [ Navbar.config NavbarMsg
                |> Navbar.withAnimation
                |> Navbar.container
                |> Navbar.brand [ href "#" ] [ text "Home" ]
                |> Navbar.items
                    [ Navbar.itemLink [ href "#example" ] [ text "Example" ]
                    , Navbar.itemLink [ href "#syntax" ] [ text "Syntax" ]
                    , Navbar.itemLink [ href "#imprint" ] [ text "Imprint" ]
                    ]
                |> Navbar.view model.navState
            , Grid.container [] <|
                case model.page of
                    Home search paging results ->
                        pageHome search paging results

                    Example results ->
                        Results.view ResultsMsg results

                    Syntax ->
                        pageSyntax

                    Imprint ->
                        pageImprint

                    NotFound ->
                        pageNotFound
            ]
        ]
    }


pageHome : Search.State -> Paging.State -> Results.State -> List (Html Msg)
pageHome search paging results =
    [ div []
        [ Grid.row []
            [ Grid.col [ Col.lg6 ]
                [ h1 []
                    [ text
                        ("Search"
                            ++ (case results of
                                    Results.Empty ->
                                        ""

                                    _ ->
                                        " - " ++ String.fromInt paging.totalResults ++ " Results"
                               )
                        )
                    ]
                ]
            ]
        , br [] []
        , Grid.row [] [ Grid.col [] (Search.view search (Search.Config SearchMsg QueryChanged Batch ExecuteQuery)) ]
        , br [] []
        , Grid.row [] [ Grid.col [] (Results.view ResultsMsg results) ]
        , br [] []
        , Grid.row [] [ Grid.col [] (Paging.view PagingMsg paging) ]
        ]
    ]


pageSyntax : List (Html msg)
pageSyntax =
    [ h1 [] [ text "Search syntax" ]
    , Grid.row [] []
    ]


pageImprint : List (Html msg)
pageImprint =
    [ h1 [] [ text "Imprint" ]
    ]


pageNotFound : List (Html msg)
pageNotFound =
    [ h1 [] [ text "Not found" ]
    , text "404 - Could not find requested page"
    ]
