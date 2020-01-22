module Main exposing (..)

import Bootstrap.Accordion as Accordion
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Bootstrap.Navbar as Navbar
import Bootstrap.Spinner as Spinner
import Browser
import Browser.Navigation as Navigation
import Entities exposing (ResultList)
import Html exposing (Html, br, div, h1, text)
import Html.Attributes exposing (href)
import Http exposing (Error(..), expectJson, jsonBody)
import List exposing (indexedMap, map)
import Query exposing (AbstractFQ(..), FacetResult, Field(..), FilterTerm(..), Query(..), decode, encode)
import Search exposing (FacetEntry, State)
import Url exposing (Url)
import Url.Parser as UrlParser



-- MAIN


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



-- MODEL


type alias Model =
    { key : Navigation.Key
    , page : Page
    , navState : Navbar.State
    , apiBaseUrl : String
    , queryState : SearchState
    , selectedStates : List Accordion.State
    , searchState : Search.State
    , lastQuery : Maybe Query
    }


type Page
    = Home
    | Example
    | Syntax
    | Imprint
    | NotFound


type SearchState
    = Init
    | Searching
    | SearchError String
    | SearchResult ResultList


init : () -> Url -> Navigation.Key -> ( Model, Cmd Msg )
init _ url key =
    let
        baseUrl =
            Url.toString { url | path = "", query = Nothing, fragment = Nothing }

        ( navState, navCmd ) =
            Navbar.initialState NavbarMsg

        ( model, urlCmd ) =
            urlUpdate url (Model key Home navState baseUrl Init [] Search.init Nothing)
    in
    ( model, Cmd.batch [ urlCmd, navCmd ] )



-- UPDATE


type Msg
    = LinkClicked Browser.UrlRequest
    | UrlChanged Url
    | NavbarMsg Navbar.State
    | ExecuteQuery
    | QueryResult (Result Http.Error ResultList)
    | FacetResult (Result Http.Error FacetResult)
    | SearchMsg Search.State
    | Selected Int Accordion.State
    | Batch (List Msg)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        LinkClicked urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( model, Navigation.pushUrl model.key (Url.toString url) )

                Browser.External href ->
                    ( model, Navigation.load href )

        UrlChanged url ->
            urlUpdate url model

        NavbarMsg state ->
            ( { model | navState = state }, Cmd.none )

        ExecuteQuery ->
            let
                filterQuery =
                    Search.buildFilterQuery model.searchState

                facetQuery =
                    Search.buildFacetQuery model.searchState
            in
            if Maybe.map (\x -> filterQuery == x) model.lastQuery |> Maybe.withDefault False then
                ( model, Cmd.none )

            else
                ( { model | queryState = Searching, selectedStates = [], lastQuery = Just filterQuery }
                , Cmd.batch [ executeQuery model facetQuery, executeQuery model filterQuery ]
                )

        QueryResult (Ok results) ->
            ( { model
                | queryState = SearchResult results
                , selectedStates = map (\_ -> Accordion.initialState) results
              }
            , Cmd.none
            )

        QueryResult (Err e) ->
            ( { model
                | queryState =
                    SearchError
                        (case e of
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
                        )
              }
            , Cmd.none
            )

        Selected idx state ->
            ( { model
                | selectedStates = List.append (List.take idx model.selectedStates) (state :: List.drop (idx + 1) model.selectedStates)
              }
            , Cmd.none
            )

        SearchMsg state ->
            ( { model | searchState = state }, Cmd.none )

        FacetResult (Ok result) ->
            ( { model | searchState = Search.update model.searchState result }, Cmd.none )

        FacetResult (Err _) ->
            ( model, Cmd.none )

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


urlUpdate : Url -> Model -> ( Model, Cmd Msg )
urlUpdate url model =
    case UrlParser.parse routeParser { url | path = Maybe.withDefault "" url.fragment, fragment = Nothing } of
        Nothing ->
            ( { model | page = NotFound }
            , Cmd.none
            )

        Just page ->
            ( { model | page = page }
            , if page == Example then
                executeQuery model (FilterQuery (Filter [ ( Name, StringExpression "*gauss*" ) ]) 10)

              else
                Cmd.none
            )


routeParser : UrlParser.Parser (Page -> a) a
routeParser =
    UrlParser.oneOf
        [ UrlParser.map Home UrlParser.top
        , UrlParser.map Example (UrlParser.s "example")
        , UrlParser.map Syntax (UrlParser.s "syntax")
        , UrlParser.map Imprint (UrlParser.s "imprint")
        ]


executeQuery : Model -> Query -> Cmd Msg
executeQuery model query =
    case query of
        FilterQuery _ _ ->
            Http.post
                { url = model.apiBaseUrl ++ "/v1/search"
                , body = jsonBody (encode query)
                , expect = expectJson QueryResult Entities.decoder
                }

        FacetQuery _ _ _ ->
            Http.post
                { url = model.apiBaseUrl ++ "/v1/facet"
                , body = jsonBody (encode query)
                , expect = expectJson FacetResult decode
                }



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        (Navbar.subscriptions model.navState NavbarMsg
            :: map (\( idx, state ) -> Accordion.subscriptions state (Selected idx)) (indexedMap Tuple.pair model.selectedStates)
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
                    Home ->
                        pageHome model

                    Example ->
                        pageHome model

                    Syntax ->
                        pageSyntax

                    Imprint ->
                        pageImprint

                    NotFound ->
                        pageNotFound
            ]
        ]
    }


pageHome : Model -> List (Html Msg)
pageHome model =
    [ div []
        [ Grid.row []
            [ Grid.col [ Col.lg6 ]
                [ h1 [] [ text "Search" ] ]
            ]
        , br [] []
        , Grid.row []
            [ Grid.col []
                -- Search.view model.searchState (Search.config SearchMsg)
                (Search.Config SearchMsg Batch ExecuteQuery
                    |> Search.view model.searchState
                )
            ]
        , br [] []
        , Grid.row []
            [ Grid.col []
                (case model.queryState of
                    Init ->
                        []

                    Searching ->
                        [ Spinner.spinner [] [] ]

                    SearchError err ->
                        [ text err ]

                    SearchResult res ->
                        Entities.view res
                )
            ]
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
