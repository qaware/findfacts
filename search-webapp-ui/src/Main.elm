module Main exposing (..)

import Bootstrap.Accordion as Accordion
import Bootstrap.Button as Button
import Bootstrap.Card as Card
import Bootstrap.Card.Block as Block
import Bootstrap.Form.Input as Input
import Bootstrap.Form.InputGroup as InputGroup
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Bootstrap.Navbar as Navbar
import Bootstrap.Text as Text
import Browser
import Browser.Navigation as Navigation
import Entities exposing (ResultList, ShortResult, decoder, kindToString)
import Html exposing (Html, br, div, h1, text)
import Html.Attributes exposing (href)
import Http exposing (Error(..), expectJson, jsonBody)
import List exposing (map)
import Query exposing (FilterQuery, encode, fromString)
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
    , searchState : SearchState
    , selectedState : Accordion.State
    , searchQuery : Maybe (Result String FilterQuery)
    }


type Page
    = Home
    | Syntax
    | Imprint
    | NotFound


type SearchState
    = Init
    | Searching
    | SearchError String
    | SearchResult (List ShortResult)


init : () -> Url -> Navigation.Key -> ( Model, Cmd Msg )
init _ url key =
    let
        baseUrl =
            Url.toString { url | path = "", query = Nothing, fragment = Nothing }

        ( navState, navCmd ) =
            Navbar.initialState NavbarMsg

        ( model, urlCmd ) =
            urlUpdate url (Model key Home navState baseUrl Init Accordion.initialState Nothing)
    in
    ( model, Cmd.batch [ urlCmd, navCmd ] )



-- UPDATE


type Msg
    = LinkClicked Browser.UrlRequest
    | UrlChanged Url
    | NavbarMsg Navbar.State
    | QueryString String
    | Search
    | NewEntities (Result Http.Error ResultList)
    | Selected Accordion.State


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

        QueryString queryStr ->
            ( { model
                | searchQuery =
                    if String.isEmpty queryStr then
                        Nothing

                    else
                        Just (fromString queryStr)
              }
            , Cmd.none
            )

        Search ->
            case model.searchQuery of
                Nothing ->
                    ( model, Cmd.none )

                Just (Err e) ->
                    ( { model | searchState = SearchError e }, Cmd.none )

                Just (Ok query) ->
                    ( { model | searchState = Searching, selectedState = Accordion.initialState }, executeQuery model query )

        NewEntities (Ok results) ->
            ( { model | searchState = SearchResult results }, Cmd.none )

        NewEntities (Err e) ->
            ( { model
                | searchState =
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

        Selected state ->
            ( { model | selectedState = state }, Cmd.none )


urlUpdate : Url -> Model -> ( Model, Cmd Msg )
urlUpdate url model =
    case UrlParser.parse routeParser { url | path = Maybe.withDefault "" url.fragment, fragment = Nothing } of
        Nothing ->
            ( { model | page = NotFound }
            , Cmd.none
            )

        Just page ->
            ( { model | page = page }
            , Cmd.none
            )


routeParser : UrlParser.Parser (Page -> a) a
routeParser =
    UrlParser.oneOf
        [ UrlParser.map Home UrlParser.top
        , UrlParser.map Syntax (UrlParser.s "syntax")
        , UrlParser.map Imprint (UrlParser.s "imprint")
        ]


executeQuery : Model -> FilterQuery -> Cmd Msg
executeQuery model query =
    Http.post
        { url = model.apiBaseUrl ++ "/v1/search"
        , body = jsonBody (encode query)
        , expect = expectJson NewEntities decoder
        }



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Navbar.subscriptions model.navState NavbarMsg
        , Accordion.subscriptions model.selectedState Selected
        ]



-- VIEW


view : Model -> Browser.Document Msg
view model =
    { title = "FindFacts"
    , body =
        [ div []
            [ menu model
            , mainContent model
            ]
        ]
    }


menu : Model -> Html Msg
menu model =
    Navbar.config NavbarMsg
        |> Navbar.withAnimation
        |> Navbar.container
        |> Navbar.brand [ href "#" ] [ text "Home" ]
        |> Navbar.items
            [ Navbar.itemLink [ href "#syntax" ] [ text "Syntax" ]
            , Navbar.itemLink [ href "#imprint" ] [ text "Imprint" ]
            ]
        |> Navbar.view model.navState


mainContent : Model -> Html Msg
mainContent model =
    Grid.container [] <|
        case model.page of
            Home ->
                pageHome model

            Syntax ->
                pageSyntax

            Imprint ->
                pageImprint

            NotFound ->
                pageNotFound


pageHome : Model -> List (Html Msg)
pageHome model =
    let
        validationState =
            case model.searchQuery of
                Nothing ->
                    []

                Just (Ok _) ->
                    [ Input.success ]

                Just (Err _) ->
                    [ Input.danger ]
    in
    [ div []
        [ Grid.row []
            [ Grid.col [ Col.lg6 ]
                [ h1 [] [ text "Search" ] ]
            ]
        , br [] []
        , Grid.row []
            [ Grid.col [ Col.lg6 ]
                [ InputGroup.config
                    (InputGroup.text (List.append [ Input.placeholder "Search for", Input.onInput QueryString ] validationState))
                    |> InputGroup.successors
                        [ InputGroup.button [ Button.primary, Button.onClick Search ] [ text "Go!" ] ]
                    |> InputGroup.view
                ]
            ]
        , br [] []
        , Grid.row []
            [ Grid.col []
                (searchResults model)
            ]
        ]
    ]


searchResults : Model -> List (Html Msg)
searchResults model =
    case model.searchState of
        Init ->
            []

        Searching ->
            [ text "Searching..." ]

        SearchError err ->
            [ text ("Something went wrong! " ++ err) ]

        SearchResult res ->
            [ Accordion.config Selected
                |> Accordion.withAnimation
                |> Accordion.cards (map displayResult res)
                |> Accordion.view model.selectedState
            ]


displayResult : ShortResult -> Accordion.Card Msg
displayResult res =
    Accordion.card
        { id = res.id
        , options = []
        , header =
            Accordion.header [] <|
                Accordion.toggle []
                    [ Card.config [ Card.align Text.alignXsLeft ]
                        |> Card.header [] [ text (kindToString res.kind) ]
                        |> Card.block []
                            [ Block.text [] [ text res.shortDescription ] ]
                        |> Card.footer [] [ text (res.sourceFile ++ ": " ++ String.fromInt res.startPosition) ]
                        |> Card.view
                    ]
        , blocks =
            [ Accordion.block []
                [ Block.text [] [ text res.sourceFile ] ]
            ]
        }


pageSyntax : List (Html Msg)
pageSyntax =
    [ h1 [] [ text "Search syntax" ]
    , Grid.row [] []
    ]


pageImprint : List (Html Msg)
pageImprint =
    [ h1 [] [ text "Imprint" ]
    ]


pageNotFound : List (Html Msg)
pageNotFound =
    [ h1 [] [ text "Not found" ]
    , text "404 - Could not find requested page"
    ]
