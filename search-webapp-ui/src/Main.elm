module Main exposing (..)

import Base64.Decode as Base64Decode
import Browser
import Browser.Dom as Dom
import Browser.Events exposing (onResize)
import Browser.Navigation as Navigation
import Components.Details as Details
import Components.Index as Index
import Components.Paging as Paging
import Components.Results as Results
import Components.Search as Search
import Components.Theory as Theory
import DataTypes exposing (..)
import Html exposing (Html, a, br, div, h1, h2, p, text)
import Html.Attributes exposing (attribute, href, style)
import Html.Lazy exposing (lazy, lazy2)
import Http
import Json.Decode as Decode
import Json.Encode as Encode exposing (Value)
import List
import Material.Drawer as Drawer exposing (modalDrawerConfig)
import Material.Elevation as Elevation
import Material.Extra.Obfuscated as Obfuscated
import Material.Icon as Icon
import Material.IconButton as IconButton exposing (iconButtonConfig)
import Material.LayoutGrid as Grid
import Material.LinearProgress as LinearProgress
import Material.List as MList exposing (listItemConfig)
import Material.Theme as Theme
import Material.TopAppBar as TopAppBar exposing (topAppBarConfig)
import Material.Typography as Typography
import Pages.About as About
import Pages.Examples as Examples
import Pages.Feedback as Feedback
import Pages.Help as Help
import Task
import Url exposing (Url)
import Url.Builder as UrlBuilder
import Url.Parser as UrlParser exposing ((</>), (<?>))
import Url.Parser.Query as UrlQueryParser
import Util exposing (consIf, ite, singletonIf)



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

        ( model, urlCmd ) =
            -- Immediately update state with url
            updateFromUrl url (Model baseUrl navKey True 0 False <| Waiting initSearch)

        windowSizeCmd =
            Task.perform (\v -> WidthMsg <| floor v.viewport.width) Dom.getViewport
    in
    ( model, Cmd.batch [ urlCmd, windowSizeCmd ] )


{-| Min width for expanded top bar.
-}
topBarMinWidth =
    600


{-| Email address, encoded so it is not visible in the source code.
-}
email : String
email =
    Base64Decode.decode Base64Decode.string "aHVjaEBpbi50dW0uZGU" |> Result.withDefault ""



-- MODEL


{-| Model containing general state of the application.
-}
type alias Model =
    { apiBaseUrl : String
    , navKey : Navigation.Key
    , locked : Bool
    , width : Int
    , drawerOpen : Bool
    , page : Page
    }


{-| Page to use in model, stores state.
-}
type Page
    = Waiting (( String, List String ) -> ( Page, Cmd Msg ))
    | Error String
    | Home PageHome
    | Details PageDetails
    | Theory PageTheory
    | Help
    | Examples
    | Feedback Obfuscated.State
    | About
    | NotFound


type alias PageHome =
    { index : Index.State
    , search : Search.State
    , paging : Paging.State
    , results : Results.State
    }


type alias PageDetails =
    { index : String
    , state : Details.State
    }


type alias PageTheory =
    { index : String
    , state : Theory.State
    }



-- UPDATE


{-| Msg type.
-}
type Msg
    = -- Routing
      UrlChanged Url
    | LinkClicked Browser.UrlRequest
    | DrawerMsg
    | WidthMsg Int
      -- Index component
    | IndexesResult (Result Http.Error (List String))
    | IndexMsg Index.State
      -- Search component
    | SearchInternalMsg Search.State
    | SearchMsg Search.State
    | SearchFacetResult Search.ResultFor (Result Http.Error ResultFaceting)
      -- Paging component
    | PagingMsg Paging.State
      -- Results component
    | ResultsMsg Results.State
    | FilterResult (Result Http.Error (ResultList ShortBlock))
      -- Details component
    | DetailsResult (Result Http.Error ShortBlock)
    | DetailsMsg (Maybe String) Details.State
    | DetailsEntityResult (Result Http.Error ThyEt)
    | EmailMsg Obfuscated.State
      --
    | ToDetail String
    | FindUsedByMsg String (List String)
    | FindUsesMsg String (List String)


{-| Main update loop.
-}
update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    if model.locked then
        case msg of
            UrlChanged url ->
                updateFromUrl url model

            WidthMsg width ->
                ( { model | width = width }, Cmd.none )

            _ ->
                -- only the UrlChanged event can free the locked state
                ( model, Cmd.none )

    else
        case ( msg, model.page ) of
            ( UrlChanged url, _ ) ->
                updateFromUrl url model

            ( DrawerMsg, _ ) ->
                ( { model | drawerOpen = not model.drawerOpen }, Cmd.none )

            ( WidthMsg width, _ ) ->
                ( { model | width = width }, Cmd.none )

            -- URL transitions
            ( LinkClicked urlRequest, _ ) ->
                case urlRequest of
                    Browser.Internal url ->
                        ( model, Navigation.pushUrl model.navKey (Url.toString url) )

                    Browser.External href ->
                        ( model, Navigation.load href )

            ( ToDetail id, Home home ) ->
                ( { model | locked = True }
                , Navigation.pushUrl model.navKey <| urlEncodeDetail (Index.selected home.index) id
                )

            ( ToDetail id, Details details ) ->
                ( { model | locked = True }
                , Navigation.pushUrl model.navKey <| urlEncodeDetail details.index id
                )

            ( ToDetail id, Theory theory ) ->
                ( { model | locked = True }
                , Navigation.pushUrl model.navKey <| urlEncodeDetail theory.index id
                )

            ( FindUsedByMsg block ids, Home home ) ->
                ( { model | locked = True }
                , Navigation.pushUrl model.navKey <|
                    urlEncodeHome (Index.selected home.index) (Search.initUsedBy block ids) Paging.empty
                )

            ( FindUsedByMsg block ids, Details details ) ->
                ( { model | locked = True }
                , Navigation.pushUrl model.navKey <|
                    urlEncodeHome details.index (Search.initUsedBy block ids) Paging.empty
                )

            ( FindUsesMsg block ids, Home home ) ->
                ( { model | locked = True }
                , Navigation.pushUrl model.navKey <|
                    urlEncodeHome (Index.selected home.index) (Search.initUses block ids) Paging.empty
                )

            ( FindUsesMsg block ids, Details details ) ->
                ( { model | locked = True }
                , Navigation.pushUrl model.navKey <|
                    urlEncodeHome details.index (Search.initUses block ids) Paging.empty
                )

            ( IndexMsg state, Home home ) ->
                ( { model | locked = True }
                , Navigation.pushUrl model.navKey <|
                    urlEncodeHome (Index.selected state) home.search Paging.empty
                )

            ( SearchMsg state, Home home ) ->
                let
                    -- on query change, reset paging
                    newPaging =
                        ite (Search.sameQuery home.search state) home.paging Paging.empty
                in
                ( { model | locked = True }
                , Navigation.pushUrl model.navKey <| urlEncodeHome (Index.selected home.index) state newPaging
                )

            ( PagingMsg newPaging, Home home ) ->
                ( { model | locked = True }
                , Navigation.pushUrl model.navKey (urlEncodeHome (Index.selected home.index) home.search newPaging)
                )

            -- messages that only update pages
            ( _, _ ) ->
                let
                    ( newPage, cmd ) =
                        updatePage model.apiBaseUrl msg model.page
                in
                ( { model | page = newPage }, cmd )


{-| Handles messages to update the current page.
-}
updatePage : String -> Msg -> Page -> ( Page, Cmd Msg )
updatePage apiBaseUrl msg page =
    case ( msg, page ) of
        ( IndexesResult (Err e), _ ) ->
            ( Error <| "Failed to get indexes: " ++ explainHttpError e, Cmd.none )

        ( IndexesResult (Ok []), _ ) ->
            ( Error "No index to search in", Cmd.none )

        ( IndexesResult (Ok (default :: rest)), Home home ) ->
            ( Home { home | index = Index.update (default :: rest) home.index }, Cmd.none )

        ( IndexesResult (Ok (default :: rest)), Waiting onResult ) ->
            onResult ( default, rest )

        ( DetailsResult result, Details details ) ->
            ( Details { details | state = Details.init <| Result.mapError explainHttpError result }, Cmd.none )

        ( DetailsMsg id details, Details old ) ->
            ( Details { old | state = details }
            , id |> Maybe.map (executeThyEtQuery apiBaseUrl old.index) |> Maybe.withDefault Cmd.none
            )

        ( DetailsEntityResult result, Details details ) ->
            ( Details { details | state = Details.update (Result.mapError explainHttpError result) details.state }
            , Cmd.none
            )

        ( FilterResult result, Theory theory ) ->
            ( Theory { theory | state = Theory.update (Result.mapError explainHttpError result) theory.state }
            , Cmd.none
            )

        ( SearchInternalMsg state, Home home ) ->
            ( Home { home | search = state }, Cmd.none )

        ( ResultsMsg newResults, Home home ) ->
            ( Home { home | results = newResults }, Cmd.none )

        ( FilterResult result, Home home ) ->
            case result of
                Ok res ->
                    -- The paging stores previous cursors in the URL
                    ( Home { home | paging = Paging.update res home.paging, results = Results.init (Ok res) }
                    , Cmd.none
                    )

                Err e ->
                    ( Home { home | paging = Paging.empty, results = Results.init (Err <| explainHttpError e) }
                    , Cmd.none
                    )

        ( SearchFacetResult id result, Home home ) ->
            ( Home { home | search = Search.update (Result.mapError explainHttpError result) id home.search }
            , Cmd.none
            )

        ( EmailMsg state, Feedback _ ) ->
            ( Feedback state, Cmd.none )

        ( _, _ ) ->
            ( page, Cmd.none )


{-| Parses url and updates model accordingly.
-}
updateFromUrl : Url -> Model -> ( Model, Cmd Msg )
updateFromUrl url model =
    -- For the SPA, all path is stored in the fragment.
    let
        -- Retrieve part before first "?" (the path) and after (the query/fragment)
        urlElems =
            String.split "?" (Maybe.withDefault "" url.fragment)

        path =
            List.head urlElems |> Maybe.withDefault ""

        query =
            List.drop 1 urlElems |> String.join "?"
    in
    case UrlParser.parse (routeParser model) { url | path = path, query = Just query, fragment = Nothing } of
        Nothing ->
            ( { model | page = NotFound, locked = False, drawerOpen = False }, Cmd.none )

        Just ( page, cmd ) ->
            ( { model | page = page, locked = False, drawerOpen = False }, cmd )


{-| Parser for the route. May not use fragment as that is already used for SPA routing.
Creates a page and a command, as information for both may be encoded in the url.
-}
routeParser : Model -> UrlParser.Parser (( Page, Cmd Msg ) -> a) a
routeParser model =
    UrlParser.oneOf
        [ UrlParser.map ( Waiting initSearch, executeIndexesQuery model.apiBaseUrl ) UrlParser.top
        , UrlParser.map (\q -> ( Waiting <| defaultSearch model q, executeIndexesQuery model.apiBaseUrl ))
            (UrlParser.s "search" <?> UrlQueryParser.string "q")
        , UrlParser.map (\index ( q, p ) -> parseHome model index q p)
            (UrlParser.s "search"
                </> UrlParser.string
                <?> UrlQueryParser.map2 (\q p -> ( q, p ))
                        (UrlQueryParser.string "q")
                        (UrlQueryParser.string "page")
            )
        , UrlParser.map (parseDetails model) (UrlParser.s "details" </> UrlParser.string </> UrlParser.string)
        , UrlParser.map (parseTheory model) (UrlParser.s "theory" </> UrlParser.string </> UrlParser.string)
        , UrlParser.map ( Feedback <| Obfuscated.init email, Cmd.none ) (UrlParser.s "feedback")
        , UrlParser.map ( Help, Cmd.none ) (UrlParser.s "help")
        , UrlParser.map ( Examples, Cmd.none ) (UrlParser.s "examples")
        , UrlParser.map ( About, Cmd.none ) (UrlParser.s "about")
        ]


{-| Creates an initial search page given available indexes.
As the search page is initial, no queries need to be executed yet.
There should be no redirect as that would be a strange user experience.
-}
initSearch : ( String, List String ) -> ( Page, Cmd Msg )
initSearch ( default, rest ) =
    ( Home <| PageHome (Index.init default |> Index.update (default :: rest)) Search.empty Paging.empty Results.empty
    , Cmd.none
    )


{-| Creates an search page for the default index - the index is not url-encoded.
The rest of the indexes are ignored; parsing the home will trigger a fresh index query.
Simply redirecting has the problem that 'back' won't work for the user.
-}
defaultSearch : Model -> Maybe String -> ( String, List String ) -> ( Page, Cmd Msg )
defaultSearch model q ( idx, _ ) =
    parseHome model idx q Nothing


{-| Parses the 'home' page, and executes queries if necessary.
-}
parseHome : Model -> String -> Maybe String -> Maybe String -> ( Page, Cmd Msg )
parseHome model index maybeSearch maybePaging =
    case maybeSearch of
        Just searchJson ->
            case
                ( searchJson |> Decode.decodeString Search.decoder
                , maybePaging
                    |> Maybe.withDefault (Encode.encode 0 <| Paging.encode Paging.empty)
                    |> Decode.decodeString Paging.decoder
                )
            of
                ( Ok parsedSearch, Ok newPaging ) ->
                    mergeHome model index parsedSearch newPaging

                _ ->
                    ( NotFound, Cmd.none )

        Nothing ->
            ( NotFound, Cmd.none )


{-| Build home page, possibly by merging with old state.
-}
mergeHome : Model -> String -> Search.State -> Paging.State -> ( Page, Cmd Msg )
mergeHome model parsedIndex parsedSearch paging =
    case model.page of
        Home old ->
            if Index.selected old.index == parsedIndex then
                -- Merge with old state
                let
                    ( search, searchState ) =
                        Search.merge old.search parsedSearch

                    index =
                        Index.select parsedIndex old.index
                in
                case searchState of
                    Search.Outdated fqs ->
                        -- Whole search is outdated, so execute filter / facet queries. Index is still ok.
                        ( Home <| PageHome index search paging Results.searching
                        , Cmd.batch
                            ((executeFilterQuery model.apiBaseUrl parsedIndex <| Paging.buildFilterQuery fqs paging)
                                :: List.map (\( q, for ) -> executeFacetQuery model.apiBaseUrl parsedIndex for q)
                                    (Search.buildFacetQueries search)
                            )
                        )

                    Search.NewFieldSearcherFacet q ->
                        -- Search is still good, but information for a new facet is missing
                        ( Home { old | index = index, search = search }
                        , executeFacetQuery model.apiBaseUrl parsedIndex Search.ForFieldSearcher q
                        )

                    Search.UpToDate fqs ->
                        -- Search is up-to-date, but page might not be
                        if Paging.samePage old.paging paging then
                            -- Same page, don't execute anything
                            ( Home { old | index = index, search = search }, Cmd.none )

                        else
                            -- Page not up to date, get new results
                            ( Home <| PageHome index search paging Results.searching
                            , executeFilterQuery model.apiBaseUrl parsedIndex <| Paging.buildFilterQuery fqs paging
                            )

            else
                -- Different index
                buildHome model.apiBaseUrl parsedIndex parsedSearch paging

        _ ->
            -- No old state
            buildHome model.apiBaseUrl parsedIndex parsedSearch paging


{-| Build new home page, executing lots of queries.
-}
buildHome : String -> String -> Search.State -> Paging.State -> ( Page, Cmd Msg )
buildHome apiBaseUrl index search paging =
    ( Home <| PageHome (Index.init index) search paging Results.searching
    , Cmd.batch
        (executeFilterQuery apiBaseUrl index (Paging.buildFilterQuery (Search.getFilters search) paging)
            :: ((Search.buildFacetQueries search |> List.map (\( q, for ) -> executeFacetQuery apiBaseUrl index for q))
                    ++ [ executeIndexesQuery apiBaseUrl ]
               )
        )
    )


{-| Parses the url for the 'details' page, and executes query.
-}
parseDetails : Model -> String -> String -> ( Page, Cmd Msg )
parseDetails model index id =
    ( Details { index = index, state = Details.empty }, executeBlockQuery model.apiBaseUrl index id )


{-| Parses the url for the 'theory' page, and executes query.
-}
parseTheory : Model -> String -> String -> ( Page, Cmd Msg )
parseTheory model index name =
    ( Theory { index = index, state = Theory.empty name }
    , executeFilterQuery model.apiBaseUrl index <| FilterQuery [ FieldFilter SrcFile <| Exact name ] 10000 Nothing
    )


{-| Encodes search state as url.
-}
urlEncodeHome : String -> Search.State -> Paging.State -> String
urlEncodeHome index search paging =
    UrlBuilder.absolute [ "#search", index ]
        ([ UrlBuilder.string "q" (search |> Search.encode |> Encode.encode 0) ]
            |> consIf (not (Paging.isEmpty paging)) (UrlBuilder.string "page" (paging |> Paging.encode |> Encode.encode 0))
        )


urlEncodeDetail : String -> String -> String
urlEncodeDetail index id =
    UrlBuilder.absolute [ "#details", index, id ] []



-- QUERIES


executeIndexesQuery : String -> Cmd Msg
executeIndexesQuery apiBaseUrl =
    Http.get { url = apiBaseUrl ++ "/v1/indexes", expect = Http.expectJson IndexesResult (Decode.list Decode.string) }


{-| Builds the command to execute a facet query.
-}
executeFacetQuery : String -> String -> Search.ResultFor -> FacetQuery -> Cmd Msg
executeFacetQuery apiBaseUrl index resultFor query =
    Http.post
        { url = apiBaseUrl ++ UrlBuilder.absolute [ "v1", index, "facet" ] []
        , body = Http.jsonBody (encodeFacetQuery query)
        , expect = Http.expectJson (SearchFacetResult resultFor) resultFacetingDecoder
        }


{-| Builds the command to execute a filter query.
-}
executeFilterQuery : String -> String -> FilterQuery -> Cmd Msg
executeFilterQuery apiBaseUrl index query =
    Http.post
        { url = apiBaseUrl ++ UrlBuilder.absolute [ "v1", index, "search" ] []
        , body = Http.jsonBody (encodeFilterQuery query)
        , expect = Http.expectJson FilterResult (resultListDecoder shortBlockDecoder)
        }


{-| Builds the command to execute a entity query.
-}
executeBlockQuery : String -> String -> String -> Cmd Msg
executeBlockQuery apiBaseUrl index blockId =
    Http.get
        { url = apiBaseUrl ++ UrlBuilder.absolute [ "v1", index, "entities", "block", "short", blockId ] []
        , expect = Http.expectJson DetailsResult shortBlockDecoder
        }


{-| Builds the command to execute a query for a single entity by id.
-}
executeThyEtQuery : String -> String -> String -> Cmd Msg
executeThyEtQuery apiBaseUrl index entityId =
    Http.get
        { url = apiBaseUrl ++ UrlBuilder.absolute [ "v1", index, "entities", "theory", "resolved", entityId ] []
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
            "Could not read response: " ++ body



-- SUBSCRIPTIONS


{-| All subscriptions.
-}
subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        ([ onResize (\w -> always <| WidthMsg w) ]
            ++ (case model.page of
                    Home home ->
                        [ Search.config SearchInternalMsg SearchMsg |> Search.subscriptions home.search ]

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
        [ div (style "height" "100%" :: ite model.locked [ attribute "pointer-events" "none" ] [])
            [ lazy renderDrawer model.drawerOpen
            , Drawer.drawerScrim [] []
            , div [ Drawer.appContent, style "height" "100%" ]
                [ lazy renderTopBar model.width, lazy2 renderPage model.width model.page ]
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
            [ MList.list MList.listConfig
                [ MList.listItem { listItemConfig | href = Just "#" } [ text "Search" ]
                , MList.listItem { listItemConfig | href = Just "#help" }
                    [ MList.listItemGraphic [] [ Icon.icon Icon.iconConfig "help" ], text "Help" ]
                , MList.listItem { listItemConfig | href = Just "#examples" }
                    [ MList.listItemGraphic [] [ Icon.icon Icon.iconConfig "search" ], text "Examples" ]
                , MList.listItem { listItemConfig | href = Just "#feedback" }
                    [ MList.listItemGraphic [] [ Icon.icon Icon.iconConfig "question_answer" ], text "Feedback" ]
                , MList.listItem { listItemConfig | href = Just "#about" }
                    [ MList.listItemGraphic [] [ Icon.icon Icon.iconConfig "info" ], text "About" ]
                ]
            ]
        ]


{-| Renders the top bar.
-}
renderTopBar : Int -> Html Msg
renderTopBar width =
    TopAppBar.topAppBar { topAppBarConfig | dense = True, additionalAttributes = [ Elevation.z4 ] }
        [ TopAppBar.row
            [ style "max-width" "1200px", style "margin" "0 auto" ]
            [ TopAppBar.section [ TopAppBar.alignStart ]
                (ite (width > topBarMinWidth)
                    []
                    [ IconButton.iconButton
                        { iconButtonConfig
                            | additionalAttributes = [ TopAppBar.navigationIcon ]
                            , onClick = Just DrawerMsg
                        }
                        "menu"
                    ]
                    ++ [ a [ href "#", TopAppBar.title, Theme.onPrimary ] [ text "FindFacts" ] ]
                )
            , TopAppBar.section [ TopAppBar.alignEnd ]
                (ite (width <= topBarMinWidth)
                    []
                    [ renderLink "search" "#"
                    , renderLink "help" "#help"
                    , renderLink "examples" "#examples"
                    , renderLink "feedback" "#feedback"
                    , renderLink "about" "#about"
                    ]
                )
            ]
        ]


renderLink : String -> String -> Html msg
renderLink name target =
    a [ href target, style "margin" "0 16px", Typography.button, Theme.onPrimary ] [ text name ]


{-| Renders the content page.
-}
renderPage : Int -> Page -> Html Msg
renderPage width page =
    case page of
        Waiting _ ->
            [ LinearProgress.indeterminateLinearProgress LinearProgress.linearProgressConfig ] |> renderInPage []

        Home home ->
            renderPageHome width home |> renderInPage [ style "min-width" "360px" ]

        Details details ->
            [ lazy2 Details.view details.state (Details.config details.index DetailsMsg ToDetail FindUsedByMsg FindUsesMsg) ]
                |> lazy (renderInPage [])

        Theory theory ->
            [ lazy2 Theory.view theory.state (Theory.config ToDetail) ]
                |> lazy (renderInPage [])

        Help ->
            Help.view |> renderInPage []

        Examples ->
            Examples.view |> renderInPage []

        Feedback emailState ->
            Feedback.config EmailMsg |> lazy2 Feedback.view emailState |> (List.singleton >> renderInPage [])

        About ->
            About.view |> renderInPage []

        NotFound ->
            renderPageNotFound |> renderInPage []

        Error error ->
            renderPageError error |> renderInPage []


{-| Renders content in a page environment.
-}
renderInPage : List (Html.Attribute Msg) -> List (Html Msg) -> Html Msg
renderInPage additionalAttrs content =
    div ([ style "min-height" "100%" ] ++ additionalAttrs)
        [ content
            |> Grid.layoutGrid
                [ style "max-width" "1200px"
                , style "margin" "0 auto"
                , style "min-height" "100%"
                , TopAppBar.denseFixedAdjust
                ]
        ]


{-| Renders the main home page.
-}
renderPageHome : Int -> PageHome -> List (Html Msg)
renderPageHome width home =
    (if width > topBarMinWidth then
        div [ style "float" "right", style "max-width" "260px", style "margin-top" "16px", style "margin-left" "32px" ]
            [ lazy2 Index.view home.index (Index.config IndexMsg) ]

     else
        div [ style "margin" "16px 0" ]
            [ Index.config IndexMsg
                |> Index.withAdditional [ style "width" "100%" ]
                |> lazy2 Index.view home.index
            ]
    )
        :: ([ h1 [ Typography.headline3 ] [ text "Search" ]
            , lazy2 Search.view home.search (Search.config SearchInternalMsg SearchMsg)
            , br [] []
            ]
                ++ singletonIf (Results.hasResults home.results)
                    (h2 [ Typography.headline4 ]
                        [ text <| String.fromInt (Paging.numResults home.paging) ++ " Blocks Found" ]
                    )
                ++ [ lazy2 Results.view home.results (Results.config ResultsMsg ToDetail FindUsedByMsg FindUsesMsg)
                   , br [] []
                   , lazy2 Paging.view home.paging (Paging.config PagingMsg)
                   ]
           )


{-| Renders the error 404 page.
-}
renderPageNotFound : List (Html msg)
renderPageNotFound =
    [ h1 [ Typography.headline1 ] [ text "Not found" ]
    , div [ Typography.headline2 ] [ text "404 - Could not find requested page" ]
    ]


{-| Renders an error page with a message.
-}
renderPageError : String -> List (Html msg)
renderPageError error =
    [ h1 [ Typography.headline1 ] [ text "Error" ]
    , div [ Typography.headline4 ] [ text error ]
    ]
