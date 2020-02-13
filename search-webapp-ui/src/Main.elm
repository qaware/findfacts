module Main exposing (..)

import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Bootstrap.Navbar as Navbar
import Browser
import Browser.Navigation as Navigation
import DataTypes exposing (..)
import Debug exposing (log)
import Html exposing (Html, br, div, h1, text)
import Html.Attributes exposing (href)
import Http
import Json.Decode as Decode
import Json.Encode as Encode exposing (Value)
import List
import PagingComponent as Paging
import ResultsComponent as Results
import SearchComponent as Search
import Url exposing (Url)
import Url.Builder as UrlBuilder
import Url.Parser as UrlParser exposing ((</>), (<?>))
import Url.Parser.Query as UrlQueryParser
import Util exposing (consIf, toMaybe)



-- APPLICATION


{-| Main application entry point.
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


{-| Application initializer.
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


{-| Model containing general state of the application.
-}
type alias Model =
    { apiBaseUrl : String
    , navKey : Navigation.Key
    , navState : Navbar.State
    , page : Page
    }


{-| Page to use in model, stores state.
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


{-| Msg type.
-}
type Msg
    = -- Routing
      UrlChanged Url
    | LinkClicked Browser.UrlRequest
    | NavbarMsg Navbar.State
      -- Search component
    | SearchInternalMsg Search.State
    | SearchMsg Search.State
    | SearchFacet FacetQuery Int
    | SearchFacetResult Int (Result Http.Error FacetResult)
      -- Paging component
    | PagingMsg Paging.State
      -- Results component
    | ResultsMsg Results.State
    | ResultsDetailMsg String
    | ResultsUsingMsg (List String)
    | FilterResult (Result Http.Error (ResultList ShortCmd))
    | DetailResult (Result Http.Error ThyEt)


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

        ( ResultsDetailMsg entityId, _ ) ->
            ( model, executeEntityQuery model.apiBaseUrl entityId )

        ( SearchInternalMsg state, Home _ paging results ) ->
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
            , Navigation.pushUrl model.navKey (urlEncodeHome newSearch newPaging)
            )

        ( PagingMsg newPaging, Home search paging results ) ->
            -- Store old state in locked page
            ( { model | page = Locked search paging results }
            , Navigation.pushUrl model.navKey (urlEncodeHome search newPaging)
            )

        ( ResultsMsg newResults, Home search paging _ ) ->
            ( { model | page = Home search paging newResults }, Cmd.none )

        ( SearchFacet facetQuery id, _ ) ->
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
                    ( { model | page = Home { search | lastQuery = Nothing } Paging.empty (Results.init (Err (explainHttpError e))) }
                    , Cmd.none
                    )

        ( SearchFacetResult id result, Home search paging results ) ->
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

        ( DetailResult result, Home search paging results ) ->
            ( { model | page = Home search paging (Results.update (Result.mapError explainHttpError result) results) }
            , Cmd.none
            )

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
                    if oldSearch.lastQuery == Just fq && Paging.samePage oldPaging paging then
                        -- Page was loaded before, so re-use results if filter and page didn't change
                        ( { model | page = Home newSearch oldPaging oldResults }, Cmd.none )

                    else
                        ( { model | page = Home newSearch paging Results.searching }
                        , executeFilterQuery model.apiBaseUrl (Paging.buildFilterQuery fq paging)
                        )

                _ ->
                    -- Page is load for the first time, so definitely query!
                    ( { model | page = Home newSearch paging Results.searching }
                    , executeFilterQuery model.apiBaseUrl (Paging.buildFilterQuery fq paging)
                    )

        Just (Example _) ->
            ( { model | page = Example Results.searching }
            , executeFilterQuery model.apiBaseUrl
                (FilterQuery (Filter [ ( Name, Term "*gauss*" ) ]) 10 Nothing)
            )

        Just page ->
            ( { model | page = page }, Cmd.none )


{-| Encodes search state as url.
-}
urlEncodeHome : Search.State -> Paging.State -> String
urlEncodeHome search paging =
    UrlBuilder.absolute [ "#search" ]
        ([ UrlBuilder.string "q" (search |> Search.encode |> Encode.encode 0) ]
            |> consIf (not (Paging.isEmpty paging)) (UrlBuilder.string "page" (paging |> Paging.encode |> Encode.encode 0))
        )


{-| Recovers home page state from strings.
-}
homeFromStrings : Maybe String -> Maybe String -> Page
homeFromStrings maybeSearch maybePaging =
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


{-| Parser for the route. May not use fragment as that is already used for SPA routing.
-}
routeParser : UrlParser.Parser (Page -> a) a
routeParser =
    UrlParser.oneOf
        [ UrlParser.map (Home Search.init Paging.empty Results.empty) UrlParser.top
        , UrlParser.s "search"
            <?> UrlQueryParser.map2 homeFromStrings
                    (UrlQueryParser.string "q")
                    (UrlQueryParser.string "page")
        , UrlParser.map (Example Results.empty) (UrlParser.s "example")
        , UrlParser.map Syntax (UrlParser.s "syntax")
        , UrlParser.map Imprint (UrlParser.s "imprint")
        ]


{-| Builds the command to execute a facet query.
-}
executeFacetQuery : String -> FacetQuery -> Int -> Cmd Msg
executeFacetQuery apiBaseUrl query id =
    Http.post
        { url = apiBaseUrl ++ "/v1/facet"
        , body = Http.jsonBody (encodeFacetQuery query)
        , expect = Http.expectJson (SearchFacetResult id) facetResultDecoder
        }


{-| Builds the command to execute a filter query.
-}
executeFilterQuery : String -> FilterQuery -> Cmd Msg
executeFilterQuery apiBaseUrl query =
    Http.post
        { url = apiBaseUrl ++ "/v1/search"
        , body = Http.jsonBody (encodeFilterQuery query)
        , expect = Http.expectJson FilterResult (resultListDecoder shortCmdDecoder)
        }


{-| Builds the command to execute a query for a single entity by id.
-}
executeEntityQuery : String -> String -> Cmd Msg
executeEntityQuery apiBaseUrl entityId =
    Http.get
        { url = apiBaseUrl ++ "/v1/resolved/" ++ entityId
        , expect = Http.expectJson DetailResult thyEtDecoder
        }


explainHttpError : Http.Error -> String
explainHttpError error =
    case error of
        Http.BadUrl _ ->
            "Invalid backend configuration"

        Http.Timeout ->
            "Backend timed out"

        Http.NetworkError ->
            "Could not reach server"

        Http.BadStatus status ->
            "Server error: " ++ String.fromInt status

        Http.BadBody body ->
            "Could not read response" ++ body



-- SUBSCRIPTIONS


{-| All subscriptions.
-}
subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        (Navbar.subscriptions model.navState NavbarMsg
            :: (case model.page of
                    Home searcher _ _ ->
                        [ Search.subscriptions searcher SearchInternalMsg ]

                    _ ->
                        []
               )
        )



-- VIEW


{-| Renders the view.
-}
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
                        Results.config ResultsMsg ResultsDetailMsg ResultsUsingMsg |> Results.view results

                    Syntax ->
                        pageSyntax

                    Imprint ->
                        pageImprint

                    NotFound ->
                        pageNotFound
            ]
        ]
    }


{-| Renders the main home page.
-}
pageHome : Search.State -> Paging.State -> Results.State -> List (Html Msg)
pageHome search paging results =
    [ div []
        [ Grid.row []
            [ Grid.col [ Col.lg6 ]
                [ h1 []
                    [ text
                        ("Search"
                            ++ (Results.hasResults results
                                    |> toMaybe (" - " ++ String.fromInt (Paging.numResults paging) ++ " Results")
                                    |> Maybe.withDefault ""
                               )
                        )
                    ]
                ]
            ]
        , br [] []
        , Grid.row [] [ Grid.col [] (Search.view search (Search.Config SearchInternalMsg SearchMsg SearchFacet)) ]
        , br [] []
        , Grid.row []
            [ Grid.col []
                (Results.config ResultsMsg ResultsDetailMsg ResultsUsingMsg
                    |> Results.view results
                )
            ]
        , br [] []
        , Grid.row [] [ Grid.col [] (Paging.config PagingMsg |> Paging.view paging) ]
        ]
    ]


{-| Renders the 'syntax' page.
-}
pageSyntax : List (Html msg)
pageSyntax =
    [ h1 [] [ text "Search syntax" ]
    , Grid.row [] []
    ]


{-| Renders the 'imprint' page.
-}
pageImprint : List (Html msg)
pageImprint =
    [ h1 [] [ text "Imprint" ]
    ]


{-| Renders the error 404 page.
-}
pageNotFound : List (Html msg)
pageNotFound =
    [ h1 [] [ text "Not found" ]
    , text "404 - Could not find requested page"
    ]
