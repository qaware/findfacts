module Main exposing (..)

import Base64.Decode as Base64Decode
import Browser
import Browser.Navigation as Navigation
import Components.Details as Details
import Components.Obfuscated as Obfuscated
import Components.Paging as Paging
import Components.Results as Results
import Components.Search as Search
import DataTypes exposing (..)
import Html exposing (Html, a, b, br, div, h1, h2, h3, p, span, text)
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
    | Help
    | Feedback Obfuscated.State
    | About
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

                ( ToDetail id, _ ) ->
                    ( { model | state = Locked False page }, Navigation.pushUrl model.navKey <| urlEncodeDetail id )

                ( FindUsedByMsg block ids, _ ) ->
                    ( { model | state = Locked False page }
                    , Navigation.pushUrl model.navKey <| urlEncodeHome (Search.initUsedBy block ids) Paging.empty
                    )

                ( FindUsesMsg block ids, _ ) ->
                    ( { model | state = Locked False page }
                    , Navigation.pushUrl model.navKey <| urlEncodeHome (Search.initUses block ids) Paging.empty
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
            ( Home (Search.update (Result.mapError explainHttpError result) id search) paging results, Cmd.none )

        ( EmailMsg state, Feedback _ ) ->
            ( Feedback state, Cmd.none )

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
        , UrlParser.map ( Feedback <| Obfuscated.init email, Cmd.none ) (UrlParser.s "feedback")
        , UrlParser.map ( Help, Cmd.none ) (UrlParser.s "help")
        , UrlParser.map ( About, Cmd.none ) (UrlParser.s "about")
        ]


email : String
email =
    Base64Decode.decode Base64Decode.string "aHVjaEBpbi50dW0uZGU" |> Result.withDefault ""


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

                        ( newSearch, mergeResult ) =
                            Search.merge oldSearch parsedSearch
                    in
                    case mergeResult of
                        Search.Outdated fqs ->
                            ( Home newSearch newPaging Results.searching
                            , Cmd.batch
                                ((executeFilterQuery model.apiBaseUrl <| Paging.buildFilterQuery fqs newPaging)
                                    :: (Search.buildFacetQueries newSearch
                                            |> List.map (\( q, for ) -> executeFacetQuery model.apiBaseUrl for q)
                                       )
                                )
                            )

                        Search.NewFacet q ->
                            ( Home newSearch oldPaging oldResults
                            , executeFacetQuery model.apiBaseUrl Search.NewFieldSearcher q
                            )

                        Search.UpToDate fqs ->
                            if Paging.samePage oldPaging newPaging then
                                ( Home newSearch oldPaging oldResults, Cmd.none )

                            else
                                ( Home newSearch newPaging Results.searching
                                , executeFilterQuery model.apiBaseUrl <| Paging.buildFilterQuery fqs newPaging
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
        , expect = Http.expectJson FilterResult (resultListDecoder shortBlockDecoder)
        }


{-| Builds the command to execute a entity query.
-}
executeBlockQuery : String -> String -> Cmd Msg
executeBlockQuery apiBaseUrl blockId =
    Http.get
        { url = apiBaseUrl ++ "/v1/entities/block/short/" ++ blockId
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
        [ div (style "height" "100%" :: ite locked [ attribute "pointer-events" "none" ] [])
            [ lazy renderDrawer drawerOpen
            , Drawer.drawerScrim [] []
            , div [ Drawer.appContent, style "height" "100%" ] [ renderTopBar, lazy renderPage page ]
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
                , MList.listItem { listItemConfig | href = Just "#help" }
                    [ MList.listItemGraphic [] [ Icon.icon Icon.iconConfig "help" ], text "Help" ]
                , MList.listItem { listItemConfig | href = Just "#feedback" }
                    [ MList.listItemGraphic [] [ Icon.icon Icon.iconConfig "question_answer" ], text "Feedback" ]
                , MList.listItem { listItemConfig | href = Just "#about" }
                    [ MList.listItemGraphic [] [ Icon.icon Icon.iconConfig "info" ], text "About" ]
                ]
            ]
        ]


{-| Renders the top bar.
-}
renderTopBar : Html Msg
renderTopBar =
    TopAppBar.topAppBar { topAppBarConfig | dense = True, additionalAttributes = [ Elevation.z4 ] }
        [ TopAppBar.row
            [ style "max-width" "1200px", style "margin" "0 auto" ]
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
    case page of
        Home search paging results ->
            renderPageHome search paging results |> renderInPage [ style "min-width" "360px" ]

        Details details ->
            [ Details.view details (Details.config DetailsMsg ToDetail FindUsedByMsg FindUsesMsg) ] |> renderInPage []

        Help ->
            renderPageHelp |> renderInPage []

        Feedback emailState ->
            renderPageFeedback emailState |> renderInPage []

        About ->
            renderPageAbout |> renderInPage []

        NotFound ->
            renderPageNotFound |> renderInPage []


{-| Renders content in a page environment.
-}
renderInPage : List (Html.Attribute Msg) -> List (Html Msg) -> Html Msg
renderInPage additionalAttrs content =
    div ([ style "background-color" "rgb(248,248,248)", style "min-height" "100%" ] ++ additionalAttrs)
        [ content
            |> Grid.layoutGrid
                [ style "background-color" "rgb(248,248,248)"
                , style "max-width" "1200px"
                , style "margin" "0 auto"
                , style "min-height" "100%"
                , TopAppBar.denseFixedAdjust
                ]
        ]


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
    , lazy2 Results.view results (Results.config ResultsMsg ToDetail FindUsedByMsg FindUsesMsg)
    , br [] []
    , lazy2 Paging.view paging (Paging.config PagingMsg)
    ]


{-| Renders the 'syntax' page.
-}
renderPageHelp : List (Html msg)
renderPageHelp =
    [ h1 [ Typography.headline3 ] [ text "Help" ]
    , p [ Typography.body1 ]
        [ text "Generally, "
        , b [] [ text "Inputs" ]
        , text " are split into "
        , b [] [ text "terms" ]
        , text " by special characters, such as whitespace, '.', '_', or '-'."
        ]
    , h2 [ Typography.headline4 ] [ text "Isabelle Characters" ]
    , p [ Typography.body1 ]
        [ text "To search for isabelle characters, use the abbreviation (if unique): "
        , a [ href "#search?q={\"term\"%3A\"%3D%3D>\"}" ] [ text "==>" ]
        , text ", the isabelle markup : "
        , a [ href "#search?page=[]&q={\"term\"%3A\"\\\\<Longrightarrow>\"}" ] [ text "\\<Longrightarrow>" ]
        , text ", or the unicode representation: "
        , a [ href "#search?page=[]&q={\"term\"%3A\"⟹\"}" ] [ text "⟹" ]
        , text "."
        ]
    , h2 [ Typography.headline4 ] [ text "Main Search Bar" ]
    , p [ Typography.body1 ]
        [ text "The main search bar will match for any of your search terms - listings results first where multiple terms match."
        , br [] []
        , text "'*' Wildcards are allowed so you don't need to be too specific."
        ]
    , h3 [ Typography.headline6 ] [ text "Example" ]
    , a [ Typography.typography, href "#search?page=[]&q={\"term\"%3A\"inv*\"}" ]
        [ text "Searching for inverse, which might be abbreviated by 'inv'" ]
    , h2 [ Typography.headline4 ] [ text "Filters" ]
    , p [ Typography.body1 ]
        [ text "You can add filters that restrict your results further."
        , br [] []
        , text "Filters always target a specific field, and they will restrict results to match either of your inputs. However, for an input to match, all its terms must match."
        ]
    , h3 [ Typography.headline6 ] [ text "Example" ]
    , a [ Typography.typography, href "#search?page=[]&q={\"fields\"%3A[{\"field\"%3A\"Name\"%2C\"terms\"%3A[\"equal nat\"%2C\"equal integer\"]}]}" ]
        [ text "Filtering for semantic entities with that are (from their name) about equality in integers or nats." ]
    , h2 [ Typography.headline4 ] [ text "Facets" ]
    , p [ Typography.body1 ]
        [ text "If you have restricted your search enough so there are only a handful of alternatives for a property, you can choose between the remaining options."
        , br [] []
        , text "Selecting multiple values will give you results that match either."
        ]
    , h3 [ Typography.headline6 ] [ text "Example" ]
    , a [ Typography.typography, href "#search?page=[]&q={\"term\"%3A\"*\"%2C\"facets\"%3A{\"Kind\"%3A[\"Constant\"]}}" ]
        [ text "Restricting search to constants" ]
    ]


{-| Renders the 'feedback' page
-}
renderPageFeedback : Obfuscated.State -> List (Html Msg)
renderPageFeedback obfuscated =
    [ h1 [ Typography.headline3 ] [ text "Feedback" ]
    , p [ Typography.body1 ]
        [ text "All feedback is greatly appreciated! Also feel free to ask any questions." ]
    , p [ Typography.body1 ]
        [ text "Simply write a message to "
        , Obfuscated.config EmailMsg
            |> Obfuscated.withDisplay
                (\s ->
                    a [ href <| "mailto:" ++ s ++ "?subject=[FindFacts] Feedback..." ]
                        [ text s ]
                )
            |> Obfuscated.withObfuscate (\e -> a [ href "" ] [ e ])
            |> Obfuscated.view obfuscated
        , text ". If you have a specific question, please include the URL!"
        ]
    , p [ Typography.body1 ]
        [ text "Please also consider filling out this short "
        , a [ href "https://forms.gle/K7Dmae9m5uVViPb57" ] [ text "survey" ]
        , text ". It won't take more than five minutes."
        ]
    ]


{-| Renders the 'about' page.
-}
renderPageAbout : List (Html msg)
renderPageAbout =
    [ h1 [ Typography.headline3 ] [ text "About" ]
    , p [ Typography.body1 ]
        [ text "This is a search application to find formal theory content of "
        , a [ href "https://isabelle.in.tum.de/" ] [ text "Isabelle" ]
        , text " and the "
        , a [ href "https://www.isa-afp.org/" ] [ text "AFP" ]
        , text "."
        ]
    , p [ Typography.body1 ]
        [ text "The development is part of my master thesis at the "
        , a [ href "http://www21.in.tum.de/index" ] [ text "Chair for Logic and Verification" ]
        , text " at TUM, in cooperation with "
        , a [ href "https://www.qaware.de/" ] [ text "QAware Software Engineering" ]
        , text "."
        ]
    , p [ Typography.body1 ]
        [ text "Source code can be found in the "
        , a [ href "https://github.com/qaware/isabelle-afp-search" ] [ text "github repository" ]
        , text "."
        ]
    , h2 [ Typography.headline5 ] [ text "Status" ]
    , p [ Typography.body1 ] [ text "The application is still ", b [] [ text "work in progress" ], text "." ]
    , p [ Typography.body1 ]
        [ text "If you encounter any bugs, unexpected behaviour, unclear UI, or have any suggestions, please leave some "
        , a [ href "#feedback" ] [ text "feedback" ]
        , text "."
        ]
    , h2 [ Typography.headline5 ] [ text "Versions" ]
    , p [ Typography.body1 ]
        [ text "Currently indexed: Isabelle/"
        , a [ href "https://isabelle.in.tum.de/repos/isabelle/rev/3548d54ce3ee" ] [ text "3548d54ce3ee" ]
        , text " and AFP/"
        , a [ href "https://bitbucket.org/isa-afp/afp-devel/src/5c040dde3c398a84fc0b1a5184dada5d2a137ab3/" ]
            [ text "5c040dde3c39" ]
        , text "."
        ]
    ]


{-| Renders the error 404 page.
-}
renderPageNotFound : List (Html msg)
renderPageNotFound =
    [ h1 [ Typography.headline1 ] [ text "Not found" ]
    , div [ Typography.headline2 ] [ text "404 - Could not find requested page" ]
    ]
