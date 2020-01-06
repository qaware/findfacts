module Main exposing (..)

import Bootstrap.Button as Button
import Bootstrap.Card as Card
import Bootstrap.Card.Block as Block
import Bootstrap.Form as Form
import Bootstrap.Form.Input as Input
import Bootstrap.Form.InputGroup as InputGroup
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Bootstrap.Navbar as Navbar
import Browser
import Browser.Events exposing (onClick)
import Browser.Navigation as Navigation
import Html exposing (..)
import Html.Attributes exposing (..)
import Http exposing (expectJson, jsonBody)

import Url exposing (Url)
import Url.Parser as UrlParser
import Query exposing (..)
import Entities exposing (..)


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
    , query : Maybe FilterQuery
    }


type Page
    = Home
    | Syntax
    | NotFound


init : () -> Url -> Navigation.Key -> ( Model, Cmd Msg )
init flags url key =
    let
        ( navState, navCmd ) =
            Navbar.initialState NavbarMsg

        ( model, urlCmd ) =
            urlUpdate url (Model key Home navState Nothing)
    in
    ( model, Cmd.batch [ urlCmd, navCmd ] )



-- UPDATE


type Msg
    = LinkClicked Browser.UrlRequest
    | UrlChanged Url
    | NavbarMsg Navbar.State
    | QueryString String
    | Search
    | NewEntities (Result Http.Error ShortResult)


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
            case fromString queryStr of
                Ok query ->
                    ( { model | query = Just query }, Cmd.none )

                Err _ ->
                    ( { model | query = Nothing }, Cmd.none )

        Search ->
            case model.query of
                Nothing ->
                    ( model, Cmd.none )

                Just q ->
                    ( model, executeQuery q )

        NewEntities res ->
            ( model, Cmd.none )


executeQuery : FilterQuery -> Cmd Msg
executeQuery query =
    Http.post
        { url = "/v1/query"
        , body = jsonBody (encode query)
        , expect = expectJson NewEntities shortDecoder
        }


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
        ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Navbar.subscriptions model.navState NavbarMsg



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
            ]
        |> Navbar.view model.navState


mainContent : Model -> Html Msg
mainContent model =
    Grid.container [] <|
        case model.page of
            Home ->
                pageHome model

            Syntax ->
                pageSyntax model

            NotFound ->
                pageNotFound


pageHome : Model -> List (Html Msg)
pageHome model =
    [ div []
        [ Grid.row []
            [ Grid.col [ Col.lg6 ]
                [ h1 [] [ text "Search" ] ]
            ]
        , br [] []
        , Grid.row []
            [ Grid.col [ Col.lg6 ]
                [ InputGroup.config
                    (InputGroup.text [ Input.placeholder "Search for", Input.onInput QueryString ])
                    |> InputGroup.successors
                        [ InputGroup.button [ Button.secondary ] [ text "Go!" ] ]
                    |> InputGroup.view
                ]
            ]
        ]
    ]


pageSyntax : Model -> List (Html Msg)
pageSyntax model =
    [ h1 [] [ text "Search syntax" ]
    , Grid.row [] []
    ]


pageNotFound : List (Html Msg)
pageNotFound =
    [ h1 [] [ text "Not found" ]
    , text "404 - Could not find requested page"
    ]
