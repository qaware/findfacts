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
import Json.Decode as Decode
import Json.Encode as Encode exposing (Value)
import List
import PagingComponent as Paging
import Query exposing (AbstractFQ(..), FacetQuery, FacetResult, FilterQuery, FilterTerm(..))
import ResultsComponent as Results
import SearchComponent as Search
import Url exposing (Url)
import Url.Builder as UrlBuilder
import Url.Parser as UrlParser exposing ((</>), (<?>))
import Url.Parser.Query as UrlQueryParser
import Util exposing (consIf)



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
            urlUpdate url (Model baseUrl key navState (Home Search.init Paging.empty Results.empty))
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
      -- The Home page is updated via url, but as urlChange is an external event, the state needs to be locked until it occurs.
    | Locked Search.State Paging.State Results.State
    | Example Results.State
    | Syntax
    | Imprint
    | NotFound



-- UPDATE


type Msg
    = -- Routing
      UrlChanged Url
    | LinkClicked Browser.UrlRequest
    | NavbarMsg Navbar.State
      -- Updating state
    | InternalSearchMsg Search.State
    | SearchMsg Search.State
    | PagingMsg Paging.State
    | ResultsMsg Results.State
      -- Executing requests
    | ExecuteQuery FacetQuery Int
    | FilterResult (Result Http.Error ResultShortlist)
    | FacetResult Int (Result Http.Error FacetResult)


{-| Main update loop.
-}
update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model.page ) of
        ( UrlChanged url, _ ) ->
            urlUpdate url model

        ( _, Locked _ _ _ ) ->
            -- only the UrlChanged event can free the locked state
            ( model, Cmd.none )

        ( LinkClicked urlRequest, _ ) ->
            case urlRequest of
                Browser.Internal url ->
                    ( model, Navigation.pushUrl model.navKey (Url.toString url) )

                Browser.External href ->
                    ( model, Navigation.load href )

        ( NavbarMsg state, _ ) ->
            ( { model | navState = state }, Cmd.none )

        ( InternalSearchMsg state, Home _ paging results ) ->
            -- internal messages only change state, not url
            ( { model | page = Home state paging results }, Cmd.none )

        ( SearchMsg newSearch, Home search paging results ) ->
            -- Check if paging needs to be reset
            let
                newPaging =
                    if (Search.update newSearch |> Tuple.second |> Just) == search.lastQuery then
                        paging

                    else
                        Paging.empty
            in
            -- Store old state in locked page
            ( { model | page = Locked search paging results }
            , Navigation.pushUrl model.navKey (encodeUrl newSearch newPaging)
            )

        ( PagingMsg newPaging, Home search paging results ) ->
            -- Store old state in locked page
            ( { model | page = Locked search paging results }
            , Navigation.pushUrl model.navKey (encodeUrl search newPaging)
            )

        ( ResultsMsg newResults, Home search paging _ ) ->
            ( { model | page = Home search paging newResults }, Cmd.none )

        ( ExecuteQuery facetQuery id, _ ) ->
            ( model, executeFacetQuery model.apiBaseUrl facetQuery id )

        ( FilterResult result, Example _ ) ->
            case result of
                Ok res ->
                    ( { model | page = Example (Results.init (Ok res)) }, Cmd.none )

                Err _ ->
                    ( { model | page = Example (Results.init (Err "Error getting example")) }, Cmd.none )

        ( FilterResult result, Home search paging _ ) ->
            case result of
                Ok res ->
                    -- The paging stores previous cursors in the URL
                    ( { model | page = Home search (Paging.update res paging) (Results.init (Ok res)) }, Cmd.none )

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
                    ( { model | page = Home { search | lastQuery = Nothing } Paging.empty (Results.init (Err cause)) }
                    , Cmd.none
                    )

        ( FacetResult id result, Home search paging results ) ->
            case result of
                Ok res ->
                    ( { model | page = Home (Search.updateWithResult search res id) paging results }, Cmd.none )

                Err _ ->
                    -- TODO improve facet error strategy
                    let
                        _ =
                            log "Error in facet"
                    in
                    ( { model | page = Home search paging results }, Cmd.none )

        ( _, _ ) ->
            ( model, Cmd.none )


{-| Parses url and updates model accordingly.
-}
urlUpdate : Url -> Model -> ( Model, Cmd Msg )
urlUpdate url model =
    -- For the SPA, all path is stored in the fragment.
    let
        -- Retreive part before first "?" (the path) and after (the query/fragment)
        urlElems =
            String.split "?" (Maybe.withDefault "" url.fragment)

        path =
            List.head urlElems |> Maybe.withDefault ""

        query =
            List.drop 1 urlElems |> String.join "?"
    in
    case UrlParser.parse routeParser { url | path = path, query = Just query, fragment = Nothing } of
        Nothing ->
            ( { model | page = NotFound }
            , Cmd.none
            )

        Just (Home search paging _) ->
            let
                -- Update search to retrieve up-to-date filter
                ( newSearch, fq ) =
                    Search.update search
            in
            case model.page of
                Locked oldSearch oldPaging oldResults ->
                    if oldSearch.lastQuery == Just fq && oldPaging.previous == paging.previous then
                        -- Page was loaded before, so re-use results if filter and page didn't change
                        ( { model | page = Home newSearch oldPaging oldResults }, Cmd.none )

                    else
                        ( { model | page = Home newSearch paging Results.Searching }
                        , executeFilterQuery model.apiBaseUrl (Paging.buildFilterQuery fq paging)
                        )

                _ ->
                    -- Page is load for the first time, so definitely query!
                    ( { model | page = Home newSearch paging Results.Searching }
                    , executeFilterQuery model.apiBaseUrl (Paging.buildFilterQuery fq paging)
                    )

        Just (Example _) ->
            ( { model | page = Example Results.Searching }
            , executeFilterQuery model.apiBaseUrl
                (FilterQuery (Query.Filter [ ( Query.Name, Query.Term "*gauss*" ) ]) 10 Nothing)
            )

        Just page ->
            ( { model | page = page }, Cmd.none )


encodeUrl : Search.State -> Paging.State -> String
encodeUrl search paging =
    UrlBuilder.absolute [ "#search" ]
        ([ UrlBuilder.string "q" (search |> Search.encode |> Encode.encode 0) ]
            |> consIf (not (List.isEmpty paging.previous)) (UrlBuilder.string "page" (paging |> Paging.encode |> Encode.encode 0))
        )


decodeUrlParts : Maybe String -> Maybe String -> Page
decodeUrlParts maybeSearch maybePaging =
    case ( maybeSearch, maybePaging ) of
        ( Just searchJson, Nothing ) ->
            case Decode.decodeString Search.decoder searchJson of
                Ok search ->
                    Home search Paging.empty Results.empty

                _ ->
                    NotFound

        ( Just searchJson, Just pagingJson ) ->
            case ( Decode.decodeString Search.decoder searchJson, Decode.decodeString Paging.decoder pagingJson ) of
                ( Ok search, Ok paging ) ->
                    Home search paging Results.empty

                _ ->
                    NotFound

        _ ->
            NotFound


routeParser : UrlParser.Parser (Page -> a) a
routeParser =
    UrlParser.oneOf
        [ UrlParser.map (Home Search.init Paging.empty Results.empty) UrlParser.top
        , UrlParser.s "search"
            <?> UrlQueryParser.map2 decodeUrlParts
                    (UrlQueryParser.string "q")
                    (UrlQueryParser.string "page")
        , UrlParser.map (Example Results.empty) (UrlParser.s "example")
        , UrlParser.map Syntax (UrlParser.s "syntax")
        , UrlParser.map Imprint (UrlParser.s "imprint")
        ]


executeFacetQuery : String -> FacetQuery -> Int -> Cmd Msg
executeFacetQuery apiBaseUrl query id =
    Http.post
        { url = apiBaseUrl ++ "/v1/facet"
        , body = jsonBody (Query.encodeFacetQuery query)
        , expect = expectJson (FacetResult id) Query.decode
        }


executeFilterQuery : String -> FilterQuery -> Cmd Msg
executeFilterQuery apiBaseUrl query =
    Http.post
        { url = apiBaseUrl ++ "/v1/search"
        , body = jsonBody (Query.encodeFilterQuery query)
        , expect = expectJson FilterResult Entities.decoder
        }



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        (Navbar.subscriptions model.navState NavbarMsg
            :: (case model.page of
                    Home searcher _ _ ->
                        [ Search.subscriptions searcher InternalSearchMsg ]

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
                    Locked search paging results ->
                        -- TODO maybe lock buttons?
                        pageHome search paging results

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
                                    Results.Result count _ _ ->
                                        " - " ++ String.fromInt count ++ " Results"

                                    _ ->
                                        ""
                               )
                        )
                    ]
                ]
            ]
        , br [] []
        , Grid.row [] [ Grid.col [] (Search.view search (Search.Config InternalSearchMsg SearchMsg ExecuteQuery)) ]
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
