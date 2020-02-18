module Main exposing (..)

import Browser
import Browser.Navigation as Navigation
import DataTypes exposing (..)
import Debug exposing (log)
import DetailsComponent as Details
import Html exposing (Html, br, div, h1, span, text)
import Html.Attributes exposing (attribute, href, style)
import Html.Lazy exposing (lazy, lazy2)
import Http
import Json.Decode as Decode
import Json.Encode as Encode exposing (Value)
import List
import Material.Chips as Chips
import Material.Drawer as Drawer exposing (modalDrawerConfig)
import Material.Icon as Icon
import Material.IconButton as IconButton exposing (iconButtonConfig)
import Material.LayoutGrid as MGrid
import Material.List
import Material.TopAppBar as TopAppBar exposing (topAppBarConfig)
import PagingComponent as Paging
import ResultsComponent as Results
import SearchComponent as Search
import Url exposing (Url)
import Url.Builder as UrlBuilder
import Url.Parser as UrlParser exposing ((</>), (<?>))
import Url.Parser.Query as UrlQueryParser
import Util exposing (consIf, ite, toMaybe)



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
init _ url navKey =
    let
        baseUrl =
            Url.toString { url | path = "", query = Nothing, fragment = Nothing }

        initialState =
            Site False <| Home Search.init Paging.empty Results.empty

        ( model, urlCmd ) =
            urlUpdate url (Model baseUrl navKey initialState)
    in
    ( model, urlCmd )



-- MODEL


{-| Model containing general state of the application.
-}
type alias Model =
    { apiBaseUrl : String
    , navKey : Navigation.Key
    , state : State
    }


{-| For url updates, the page is locked temporarily until the urlChange event happens,
so no more url changes are triggered.
-}
type State
    = Locked Bool Page
    | Site Bool Page


{-| Page to use in model, stores state.
-}
type Page
    = Home Search.State Paging.State Results.State
    | Details Details.State
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
    | DrawerMsg
      -- Search component
    | SearchInternalMsg Search.State
    | SearchMsg Search.State
    | SearchFacet FacetQuery Int
    | SearchFacetResult Int (Result Http.Error FacetResult)
      -- Paging component
    | PagingMsg Paging.State
      -- Results component
    | ResultsMsg Results.State
    | ResultsDetail String
    | ResultsUsingMsg (List String)
    | FilterResult (Result Http.Error (ResultList ShortCmd))
      -- Details component
    | DetailsResult (Result Http.Error ShortBlock)
    | DetailsMsg (Maybe String) Details.State
    | DetailsEntityResult (Result Http.Error ThyEt)


{-| Main update loop.
-}
update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case model.state of
        Locked _ _ ->
            case msg of
                UrlChanged url ->
                    urlUpdate url model

                _ ->
                    -- only the UrlChanged event can free the locked state
                    ( model, Cmd.none )

        Site drawerOpen page ->
            case ( msg, page ) of
                ( UrlChanged url, _ ) ->
                    urlUpdate url model

                ( DrawerMsg, _ ) ->
                    ( { model | state = Site (not drawerOpen) page }, Cmd.none )

                -- URL transitions
                ( LinkClicked urlRequest, _ ) ->
                    case urlRequest of
                        Browser.Internal url ->
                            ( model, Navigation.pushUrl model.navKey (Url.toString url) )

                        Browser.External href ->
                            ( model, Navigation.load href )

                ( ResultsDetail id, _ ) ->
                    ( { model | state = Locked False page }, Navigation.pushUrl model.navKey <| urlEncodeDetail id )

                ( SearchMsg newSearch, Home search paging _ ) ->
                    -- Check if paging needs to be reset
                    let
                        newPaging =
                            if (Search.update newSearch |> Tuple.second |> Just) == search.lastQuery then
                                paging

                            else
                                Paging.empty
                    in
                    ( { model | state = Locked False page }
                    , Navigation.pushUrl model.navKey (urlEncodeHome newSearch newPaging)
                    )

                ( PagingMsg newPaging, Home search paging results ) ->
                    ( { model | state = Locked False (Home search paging results) }
                    , Navigation.pushUrl model.navKey (urlEncodeHome search newPaging)
                    )

                -- messages that only update pages
                ( _, _ ) ->
                    let
                        ( newPage, cmd ) =
                            updatePage model.apiBaseUrl msg page
                    in
                    ( { model | state = Site drawerOpen newPage }, cmd )


{-| Handles messages to update the current page.
-}
updatePage : String -> Msg -> Page -> ( Page, Cmd Msg )
updatePage apiBaseUrl msg page =
    case ( msg, page ) of
        ( DetailsResult result, Details _ ) ->
            ( Details <| Details.init <| Result.mapError explainHttpError result, Cmd.none )

        ( DetailsMsg id details, Details _ ) ->
            ( Details details, id |> Maybe.map (executeThyEtQuery apiBaseUrl) |> Maybe.withDefault Cmd.none )

        ( DetailsEntityResult result, Details details ) ->
            ( Details <| Details.update (Result.mapError explainHttpError result) details, Cmd.none )

        ( SearchInternalMsg state, Home _ paging results ) ->
            ( Home state paging results, Cmd.none )

        ( ResultsMsg newResults, Home search paging _ ) ->
            ( Home search paging newResults, Cmd.none )

        ( SearchFacet facetQuery id, _ ) ->
            ( page, executeFacetQuery apiBaseUrl facetQuery id )

        ( FilterResult result, Home search paging _ ) ->
            case result of
                Ok res ->
                    -- The paging stores previous cursors in the URL
                    ( Home search (Paging.update res paging) (Results.init (Ok res)), Cmd.none )

                Err e ->
                    ( Home { search | lastQuery = Nothing } Paging.empty (Results.init (Err (explainHttpError e)))
                    , Cmd.none
                    )

        ( SearchFacetResult id result, Home search paging results ) ->
            case result of
                Ok res ->
                    ( Home (Search.updateWithResult search res id) paging results, Cmd.none )

                Err _ ->
                    -- TODO improve facet error strategy
                    let
                        _ =
                            log "Error in facet"
                    in
                    ( Home search paging results, Cmd.none )

        ( _, _ ) ->
            ( page, Cmd.none )


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
    case UrlParser.parse (routeParser model) { url | path = path, query = Just query, fragment = Nothing } of
        Nothing ->
            ( { model | state = Site False NotFound }, Cmd.none )

        Just ( page, cmd ) ->
            ( { model | state = Site False page }, cmd )


{-| Parser for the route. May not use fragment as that is already used for SPA routing.
-}
routeParser : Model -> UrlParser.Parser (( Page, Cmd Msg ) -> a) a
routeParser model =
    UrlParser.oneOf
        [ UrlParser.map ( Home Search.init Paging.empty Results.empty, Cmd.none ) UrlParser.top
        , UrlParser.s "search"
            <?> UrlQueryParser.map2 (parseHome model)
                    (UrlQueryParser.string "q")
                    (UrlQueryParser.string "page")
        , UrlParser.map (parseDetails model) (UrlParser.s "details" </> UrlParser.string)
        , UrlParser.map ( Syntax, Cmd.none ) (UrlParser.s "syntax")
        , UrlParser.map ( Imprint, Cmd.none ) (UrlParser.s "imprint")
        ]


{-| Parses the 'home' page, and executes queries if necessary.
-}
parseHome : Model -> Maybe String -> Maybe String -> ( Page, Cmd Msg )
parseHome model maybeSearch maybePaging =
    case maybeSearch of
        Just searchJson ->
            case
                ( searchJson |> Decode.decodeString Search.decoder
                , maybePaging
                    |> Maybe.withDefault (Encode.encode 0 <| Paging.encode Paging.empty)
                    |> Decode.decodeString Paging.decoder
                )
            of
                ( Ok search, Ok paging ) ->
                    let
                        -- Update search to retrieve up-to-date filter
                        ( newSearch, fq ) =
                            Search.update search
                    in
                    case model.state of
                        Locked _ (Home oldSearch oldPaging oldResults) ->
                            if oldSearch.lastQuery == Just fq && Paging.samePage oldPaging paging then
                                -- Page was loaded before, so re-use results if filter and page didn't change
                                ( Home newSearch oldPaging oldResults, Cmd.none )

                            else
                                ( Home newSearch paging Results.searching
                                , Cmd.batch [ executeFilterQuery model.apiBaseUrl (Paging.buildFilterQuery fq paging) ]
                                )

                        _ ->
                            -- Page is load for the first time, so definitely query!
                            ( Home newSearch paging Results.searching
                            , executeFilterQuery model.apiBaseUrl (Paging.buildFilterQuery fq paging)
                            )

                _ ->
                    ( NotFound, Cmd.none )

        Nothing ->
            ( NotFound, Cmd.none )


{-| Parses the url for the 'details' page, and executes query.
-}
parseDetails : Model -> String -> ( Page, Cmd Msg )
parseDetails model id =
    ( Details Details.empty, executeBlockQuery model.apiBaseUrl id )


{-| Encodes search state as url.
-}
urlEncodeHome : Search.State -> Paging.State -> String
urlEncodeHome search paging =
    UrlBuilder.absolute [ "#search" ]
        ([ UrlBuilder.string "q" (search |> Search.encode |> Encode.encode 0) ]
            |> consIf (not (Paging.isEmpty paging)) (UrlBuilder.string "page" (paging |> Paging.encode |> Encode.encode 0))
        )


urlEncodeDetail : String -> String
urlEncodeDetail id =
    UrlBuilder.absolute [ "#details", id ] []


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


{-| Builds the command to execute a entity query.
-}
executeBlockQuery : String -> String -> Cmd Msg
executeBlockQuery apiBaseUrl blockId =
    Http.get
        { url = apiBaseUrl ++ "/v1/entities/cmd/short/" ++ blockId
        , expect = Http.expectJson DetailsResult shortBlockDecoder
        }


{-| Builds the command to execute a query for a single entity by id.
-}
executeThyEtQuery : String -> String -> Cmd Msg
executeThyEtQuery apiBaseUrl entityId =
    Http.get
        { url = apiBaseUrl ++ "/v1/entities/theory/resolved/" ++ entityId
        , expect = Http.expectJson DetailsEntityResult thyEtDecoder
        }


{-| Transforms http error to explanation string.
-}
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
        (case model.state of
            Site _ (Home searcher _ _) ->
                [ Search.subscriptions searcher SearchInternalMsg ]

            _ ->
                []
        )



-- VIEW


{-| Renders the view.
-}
view : Model -> Browser.Document Msg
view model =
    let
        ( page, drawerOpen, locked ) =
            case model.state of
                Site open p ->
                    ( p, open, False )

                Locked open p ->
                    ( p, open, True )
    in
    { title = "FindFacts"
    , body =
        [ div (ite locked [ attribute "pointer-events" "none" ] [])
            [ lazy renderDrawer drawerOpen
            , Drawer.drawerScrim [] []
            , div [ Drawer.appContent, style "background-color" "rgb(248,248,248)", style "min-height" "100vh" ] <|
                [ renderTopBar
                , br [] []
                , lazy renderPage page
                ]
            ]
        ]
    }


{-| Renders the menu drawer.
-}
renderDrawer : Bool -> Html Msg
renderDrawer open =
    Drawer.modalDrawer
        { modalDrawerConfig | open = open, onClose = Just DrawerMsg }
        [ Drawer.drawerContent []
            [ Material.List.list Material.List.listConfig
                [ Material.List.listItem Material.List.listItemConfig
                    [ text "Home" ]
                , Material.List.listItem Material.List.listItemConfig
                    [ Material.List.listItemGraphic [] [ Icon.icon Icon.iconConfig "star" ] ]
                , Material.List.listItem Material.List.listItemConfig
                    [ text "Log out" ]
                ]
            ]
        ]


{-| Renders the top bar.
-}
renderTopBar : Html Msg
renderTopBar =
    TopAppBar.topAppBar { topAppBarConfig | dense = True }
        [ TopAppBar.row
            [ style "max-width" "1170px", style "margin" "0 auto" ]
            [ TopAppBar.section [ TopAppBar.alignStart ]
                [ IconButton.iconButton
                    { iconButtonConfig
                        | additionalAttributes = [ TopAppBar.navigationIcon ]
                        , onClick = Just DrawerMsg
                    }
                    "menu"
                , span [ TopAppBar.title ]
                    [ text "FindFacts" ]
                ]
            ]
        ]


{-| Renders the content page.
-}
renderPage : Page -> Html Msg
renderPage page =
    MGrid.layoutGrid [ style "max-width" "1170px", style "margin" "0 auto", TopAppBar.denseFixedAdjust ] <|
        case page of
            Home search paging results ->
                renderPageHome search paging results

            Details details ->
                Details.config DetailsMsg |> Details.view details

            Syntax ->
                renderPageSyntax

            Imprint ->
                renderPageImprint

            NotFound ->
                renderPageNotFound


{-| Renders the main home page.
-}
renderPageHome : Search.State -> Paging.State -> Results.State -> List (Html Msg)
renderPageHome search paging results =
    [ div []
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
    , br [] []
    , lazy2 Search.view search (Search.Config SearchInternalMsg SearchMsg SearchFacet)
    , br [] []
    , lazy2 Results.view results (Results.config ResultsMsg ResultsDetail ResultsUsingMsg)
    , br [] []
    , lazy2 Paging.view paging (Paging.config PagingMsg)
    ]


{-| Renders the 'syntax' page.
-}
renderPageSyntax : List (Html msg)
renderPageSyntax =
    [ h1 [] [ text "Search syntax" ]
    , Chips.choiceChipSet [] [ Chips.choiceChip Chips.choiceChipConfig "chip" ]
    ]


{-| Renders the 'imprint' page.
-}
renderPageImprint : List (Html msg)
renderPageImprint =
    [ h1 [] [ text "Imprint" ]
    ]


{-| Renders the error 404 page.
-}
renderPageNotFound : List (Html msg)
renderPageNotFound =
    [ h1 [] [ text "Not found" ]
    , text "404 - Could not find requested page"
    ]
