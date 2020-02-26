module Main exposing (..)

import Browser
import Browser.Navigation as Navigation
import Components.Details as Details
import Components.Paging as Paging
import Components.Results as Results
import Components.Search as Search
import DataTypes exposing (..)
import Html exposing (Html, br, div, h1, span, text)
import Html.Attributes exposing (attribute, href, style)
import Html.Lazy exposing (lazy, lazy2)
import Http
import Json.Decode as Decode
import Json.Encode as Encode exposing (Value)
import List
import Material.Drawer as Drawer exposing (modalDrawerConfig)
import Material.Elevation as Elevation
import Material.Icon as Icon
import Material.IconButton as IconButton exposing (iconButtonConfig)
import Material.LayoutGrid as Grid
import Material.List as MList exposing (listItemConfig)
import Material.TopAppBar as TopAppBar exposing (topAppBarConfig)
import Material.Typography as Typography
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
            Site False <| Home Search.empty Paging.empty Results.empty

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
    | SearchFacetResult Search.ResultFor (Result Http.Error ResultFaceting)
      -- Paging component
    | PagingMsg Paging.State
      -- Results component
    | ResultsMsg Results.State
    | ResultsDetail String
    | ResultsUsingMsg String (List String)
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

                ( ResultsUsingMsg block ids, Home search paging _ ) ->
                    ( { model | state = Locked False page }
                    , Navigation.pushUrl model.navKey <| urlEncodeHome (Search.addUsing block ids search) paging
                    )

                ( SearchMsg newSearch, Home oldSearch paging _ ) ->
                    let
                        -- on query change, reset paging
                        newPaging =
                            ite (Search.sameQuery oldSearch newSearch) paging Paging.empty
                    in
                    ( { model | state = Locked False page }
                    , Navigation.pushUrl model.navKey <| urlEncodeHome newSearch newPaging
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

        ( SearchInternalMsg newSearch, Home _ paging results ) ->
            ( Home newSearch paging results, Cmd.none )

        ( ResultsMsg newResults, Home search paging _ ) ->
            ( Home search paging newResults, Cmd.none )

        ( FilterResult result, Home search paging _ ) ->
            case result of
                Ok res ->
                    -- The paging stores previous cursors in the URL
                    ( Home search (Paging.update res paging) (Results.init (Ok res)), Cmd.none )

                Err e ->
                    ( Home search Paging.empty <| Results.init (Err <| explainHttpError e), Cmd.none )

        ( SearchFacetResult id result, Home search paging results ) ->
            case result of
                Ok res ->
                    ( Home (Search.update res id search) paging results, Cmd.none )

                Err e ->
                    let
                        _ =
                            Debug.log "Error in facet: " e
                    in
                    -- TODO display error in searcher
                    ( page, Cmd.none )

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
        [ UrlParser.map ( Home Search.empty Paging.empty Results.empty, Cmd.none ) UrlParser.top
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
                ( Ok parsedSearch, Ok newPaging ) ->
                    let
                        ( oldSearch, oldPaging, oldResults ) =
                            case model.state of
                                Locked _ (Home oldS oldP oldR) ->
                                    ( oldS, oldP, oldR )

                                _ ->
                                    ( Search.empty, Paging.empty, Results.empty )

                        ( newSearch, maybeQuery, maybeFacet ) =
                            Search.merge oldSearch parsedSearch
                    in
                    if Search.sameQuery oldSearch newSearch && Paging.samePage oldPaging newPaging then
                        -- Page was loaded before, so re-use results if filter and page didn't change
                        ( Home newSearch oldPaging oldResults
                        , maybeFacet
                            |> Maybe.map (executeFacetQuery model.apiBaseUrl Search.NewFieldSearcher)
                            |> Maybe.withDefault Cmd.none
                        )

                    else
                        ( Home newSearch newPaging Results.searching
                        , maybeQuery
                            |> Maybe.map (executeQueries model.apiBaseUrl newSearch newPaging)
                            |> Maybe.withDefault Cmd.none
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


executeQueries : String -> Search.State -> Paging.State -> List FieldFilter -> Cmd Msg
executeQueries apiBaseUrl search paging fq =
    Cmd.batch
        ((executeFilterQuery apiBaseUrl <| Paging.buildFilterQuery fq paging)
            :: (Search.buildFacetQueries
                    search
                    |> List.map (\( query, target ) -> executeFacetQuery apiBaseUrl target query)
               )
        )


{-| Builds the command to execute a facet query.
-}
executeFacetQuery : String -> Search.ResultFor -> FacetQuery -> Cmd Msg
executeFacetQuery apiBaseUrl resultFor query =
    Http.post
        { url = apiBaseUrl ++ "/v1/facet"
        , body = Http.jsonBody (encodeFacetQuery query)
        , expect = Http.expectJson (SearchFacetResult resultFor) resultFacetingDecoder
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
            "Could not read response: " ++ body



-- SUBSCRIPTIONS


{-| All subscriptions.
-}
subscriptions : Model -> Sub Msg
subscriptions model =
    case model.state of
        Site _ (Home search _ _) ->
            Search.config SearchInternalMsg SearchMsg |> Search.subscriptions search

        _ ->
            Sub.none



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
            [ MList.list MList.listConfig
                [ MList.listItem { listItemConfig | href = Just "#" } [ text "Home" ]
                , MList.listItem { listItemConfig | href = Just "#syntax" }
                    [ MList.listItemGraphic [] [ Icon.icon Icon.iconConfig "help" ], text "Syntax" ]
                , MList.listItem { listItemConfig | href = Just "#feedback" }
                    [ MList.listItemGraphic [] [ Icon.icon Icon.iconConfig "question_answer" ], text "Feedback" ]
                ]
            ]
        ]


{-| Renders the top bar.
-}
renderTopBar : Html Msg
renderTopBar =
    TopAppBar.topAppBar { topAppBarConfig | dense = True, additionalAttributes = [ Elevation.z4 ] }
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
    Grid.layoutGrid [ style "max-width" "1170px", style "margin" "0 auto", TopAppBar.denseFixedAdjust ] <|
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
        [ h1 [ Typography.headline3 ]
            [ text
                ("Search"
                    ++ (Results.hasResults results
                            |> toMaybe (" - " ++ String.fromInt (Paging.numResults paging) ++ " Results")
                            |> Maybe.withDefault ""
                       )
                )
            ]
        ]
    , lazy2 Search.view search (Search.config SearchInternalMsg SearchMsg)
    , br [] []
    , lazy2 Results.view results (Results.config ResultsMsg ResultsDetail ResultsUsingMsg)
    , br [] []
    , lazy2 Paging.view paging (Paging.config PagingMsg)
    ]


{-| Renders the 'syntax' page.
-}
renderPageSyntax : List (Html msg)
renderPageSyntax =
    [ h1 [ Typography.headline3 ] [ text "Search syntax" ] ]


{-| Renders the 'imprint' page.
-}
renderPageImprint : List (Html msg)
renderPageImprint =
    [ h1 [ Typography.headline3 ] [ text "Imprint" ] ]


{-| Renders the error 404 page.
-}
renderPageNotFound : List (Html msg)
renderPageNotFound =
    [ h1 [ Typography.headline1 ] [ text "Not found" ]
    , div [ Typography.headline2 ] [ text "404 - Could not find requested page" ]
    ]
