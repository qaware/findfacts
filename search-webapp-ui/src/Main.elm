module Main exposing (..)

import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Bootstrap.Navbar as Navbar
import Bootstrap.Spinner as Spinner
import Browser
import Browser.Navigation as Navigation
import Entities exposing (ResultShortlist)
import Html exposing (Html, br, div, h1, text)
import Html.Attributes exposing (href)
import Http exposing (Error(..), expectJson, jsonBody)
import List
import Query exposing (AbstractFQ(..), FacetResult, Field(..), FilterTerm(..), Query(..), decode, encode)
import Results exposing (Config)
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



-- CONFIG


type alias Config =
    { apiBaseUrl : String
    }



-- MODEL


type alias Model =
    { config : Config
    , key : Navigation.Key
    , page : Page
    , navState : Navbar.State
    , searchState : Search.State
    , resultState : ResultState
    , lastQuery : Maybe Query
    }


type Page
    = Home
    | Example
    | Syntax
    | Imprint
    | NotFound


type ResultState
    = Init
    | Searching
    | SearchError String
    | SearchResult Results.State Int String


init : () -> Url -> Navigation.Key -> ( Model, Cmd Msg )
init _ url key =
    let
        baseUrl =
            Url.toString { url | path = "", query = Nothing, fragment = Nothing }

        ( navState, navCmd ) =
            Navbar.initialState NavbarMsg

        ( model, urlCmd ) =
            urlUpdate url (Model (Config baseUrl) key Home navState Search.init Init Nothing)
    in
    ( model, Cmd.batch [ urlCmd, navCmd ] )


reset : Model -> Model
reset model =
    { model | resultState = Init, searchState = Search.init, lastQuery = Nothing }



-- UPDATE


type Msg
    = LinkClicked Browser.UrlRequest
    | UrlChanged Url
    | NavbarMsg Navbar.State
    | ExecuteQuery
    | QueryResult (Result Http.Error ResultShortlist)
    | FacetResult (Result Http.Error FacetResult)
    | SearchMsg Search.State
    | ResultsMsg Results.State
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
                ( { model | resultState = Searching, lastQuery = Just filterQuery }
                , Cmd.batch [ executeQuery model.config facetQuery, executeQuery model.config filterQuery ]
                )

        QueryResult (Ok results) ->
            ( { model | resultState = SearchResult (Results.init results) results.count results.nextCursor }, Cmd.none )

        QueryResult (Err e) ->
            ( { model
                | resultState =
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

        ResultsMsg state ->
            case model.resultState of
                SearchResult res count cursor ->
                    ( { model | resultState = SearchResult state count cursor }, Cmd.none )

                _ ->
                    ( model, Cmd.none )


urlUpdate : Url -> Model -> ( Model, Cmd Msg )
urlUpdate url model =
    case UrlParser.parse routeParser { url | path = Maybe.withDefault "" url.fragment, fragment = Nothing } of
        Nothing ->
            ( { model | page = NotFound }
            , Cmd.none
            )

        Just page ->
            ( { model | page = page } |> reset
            , if page == Example then
                executeQuery model.config (FilterQuery (Filter [ ( Name, Term "*gauss*" ) ]) 10)

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


executeQuery : Config -> Query -> Cmd Msg
executeQuery config query =
    case query of
        FilterQuery _ _ ->
            Http.post
                { url = config.apiBaseUrl ++ "/v1/search"
                , body = jsonBody (encode query)
                , expect = expectJson QueryResult Entities.decoder
                }

        FacetQuery _ _ _ ->
            Http.post
                { url = config.apiBaseUrl ++ "/v1/facet"
                , body = jsonBody (encode query)
                , expect = expectJson FacetResult decode
                }


displayQueryError : Error -> Model -> Model
displayQueryError e model =
    { model
        | resultState =
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



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch (Navbar.subscriptions model.navState NavbarMsg :: Search.subscriptions model.searchState SearchMsg :: [])



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
                [ h1 []
                    [ text
                        ("Search"
                            ++ (case model.resultState of
                                    SearchResult _ count _ ->
                                        " - " ++ String.fromInt count ++ " Results"

                                    _ ->
                                        ""
                               )
                        )
                    ]
                ]
            ]
        , br [] []
        , Grid.row []
            [ Grid.col []
                (Search.Config SearchMsg Batch ExecuteQuery
                    |> Search.view model.searchState
                )
            ]
        , br [] []
        , Grid.row []
            [ Grid.col []
                (case model.resultState of
                    Init ->
                        []

                    Searching ->
                        [ Spinner.spinner [] [] ]

                    SearchError err ->
                        [ text err ]

                    SearchResult res count cursor ->
                        Results.view ResultsMsg res
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
