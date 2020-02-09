module SearchComponent exposing (Config, State, buildFQ, init, subscriptions, update, view)

import Array exposing (Array)
import Bootstrap.Badge as Badge
import Bootstrap.Button as Button
import Bootstrap.Dropdown as Dropdown
import Bootstrap.Form.Input as Input
import Bootstrap.Form.InputGroup as InputGroup
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Bootstrap.Text as Text
import Dict exposing (Dict)
import Dict.Any as AnyDict exposing (AnyDict)
import Html exposing (Html, text)
import Html.Events as Events exposing (onClick)
import Html.Events.Extra as ExtraEvents
import List exposing (map)
import Query exposing (AbstractFQ(..), FacetQuery, FacetResult, Field(..), FilterTerm(..), fieldToString)
import Tuple as Pair
import Url exposing (Url)
import Url.Parser as UrlParser exposing (Parser)



-- STATIC


facetableFields : List Field
facetableFields =
    [ CmdKind, SrcFile, Kind, ConstTypeFacet, DocKind ]


termFilterableFields : List Field
termFilterableFields =
    [ CmdKind, Src, SrcFile, Name, Kind, Prop, ConstType, DocKind ]


rangeFilterableFields =
    []



-- CONFIG


type alias Config msg =
    { toMsg : State -> msg
    , toUrl : String -> msg
    , toBatch : List msg -> msg
    , exec : FacetQuery -> Int -> msg
    }



-- STATE


{-| All state that needs to be kept track of
-}
type alias State =
    { termSearcher : String
    , facetSelectors : Faceting
    , fieldSearchers : Array FieldSearcher
    , newField : Dropdown.State
    , lastQuery : Maybe AbstractFQ
    }


type alias Facet =
    -- Name -> FacetEntry
    Dict String FacetEntry


type alias FacetEntry =
    { count : Int
    , selected : Bool
    }


type alias Faceting =
    -- Field -> (implicit dict key) -> Facet Values
    AnyDict String Field Facet


type alias FieldSearcher =
    { fieldSelect : Dropdown.State
    , field : Field
    , value : String
    , facetSelect : Maybe ( Dropdown.State, Facet )
    }


init : State
init =
    State "" (AnyDict.empty fieldToString) Array.empty Dropdown.initialState Nothing



-- UTILITIES


facetValues : Facet -> List String
facetValues f =
    Dict.filter (\_ -> .selected) f |> Dict.keys


facetSelected : Facet -> Bool
facetSelected facet =
    facet |> Dict.values |> List.any .selected


getFilterTerm : String -> FilterTerm
getFilterTerm str =
    if String.startsWith "\"" str && String.endsWith "\"" str then
        str |> String.dropLeft 1 |> String.dropRight 1 |> Exact

    else
        Term str



-- Encoding/Decoding


urlEncoder : State -> String



-- TODO


urlEncoder state =
    ""



--urlParser: UrlParser.Parser (State -> a) a
--urlParser = UrlParser.query
-- QUERYING


buildSingleFQ : Field -> FilterTerm -> AbstractFQ
buildSingleFQ field term =
    Filter [ ( field, term ) ]


buildFacetFQ : Field -> Facet -> Maybe AbstractFQ
buildFacetFQ field facet =
    case facetValues facet of
        [] ->
            Nothing

        exp :: [] ->
            Just (buildSingleFQ field (Exact exp))

        exp1 :: exp2 :: exps ->
            Just
                (Union (buildSingleFQ field (Exact exp1))
                    (buildSingleFQ field (Exact exp2))
                    (exps
                        |> map Exact
                        |> map (buildSingleFQ field)
                    )
                )


buildFieldSearcherFQ : FieldSearcher -> Maybe AbstractFQ
buildFieldSearcherFQ fieldSearcher =
    if String.isEmpty fieldSearcher.value then
        fieldSearcher.facetSelect |> Maybe.map Pair.second |> Maybe.andThen (buildFacetFQ fieldSearcher.field)

    else
        fieldSearcher.value |> getFilterTerm |> buildSingleFQ fieldSearcher.field |> Just


buildFQ : State -> AbstractFQ
buildFQ state =
    let
        termFQ =
            if String.isEmpty state.termSearcher then
                []

            else
                let
                    filterTerm =
                        getFilterTerm state.termSearcher
                in
                [ Union (buildSingleFQ Query.Name filterTerm)
                    (buildSingleFQ Query.Src filterTerm)
                    [ buildSingleFQ Query.Prop filterTerm ]
                ]

        fieldFQs =
            state.fieldSearchers |> Array.toList |> List.filterMap buildFieldSearcherFQ

        facetFQs =
            state.facetSelectors |> AnyDict.toList |> List.filterMap (\( x, y ) -> buildFacetFQ x y)
    in
    case List.append termFQ (List.append fieldFQs facetFQs) of
        [] ->
            Filter []

        fq :: [] ->
            fq

        fq1 :: fq2 :: fqn ->
            Intersection fq1 fq2 fqn


buildFacetQuery : State -> FacetQuery
buildFacetQuery state =
    FacetQuery (buildFQ state) facetableFields 10



-- UPDATE


makeFacet : Query.Facet -> Facet
makeFacet queryFacet =
    Dict.map (\_ count -> FacetEntry count False) queryFacet


updateCounts : Facet -> Query.Facet -> Facet
updateCounts facet queryFacets =
    facet
        |> Dict.map
            (\name entry ->
                case Dict.get name queryFacets of
                    Nothing ->
                        { entry | count = 0 }

                    Just some ->
                        { entry | count = some }
            )


updateFacet : Facet -> Maybe Query.Facet -> Facet
updateFacet facet queryFacet =
    case queryFacet of
        Nothing ->
            facet

        Just qFacet ->
            if facetSelected facet then
                updateCounts facet qFacet

            else
                -- re-build since no information is lost here
                makeFacet qFacet


makeFaceting : FacetResult -> Faceting
makeFaceting result =
    AnyDict.map (\_ -> makeFacet) result


updateFaceting : Faceting -> FacetResult -> Faceting
updateFaceting faceting result =
    let
        ( used, _ ) =
            AnyDict.partition (\_ -> facetSelected) faceting

        ( upd, new ) =
            AnyDict.partition (\k _ -> AnyDict.member k used) result
    in
    makeFaceting new |> AnyDict.union (AnyDict.map (\field facet -> AnyDict.get field upd |> updateFacet facet) used)


update : State -> FacetResult -> Int -> State
update state result id =
    { state | facetSelectors = updateFaceting state.facetSelectors result }



-- SUBSCRIPTIONS


subscriptions : State -> (State -> msg) -> Sub msg
subscriptions state toMsg =
    Sub.batch [ Dropdown.subscriptions state.newField (\d -> toMsg { state | newField = d }) ]



-- VIEW


changeEvents : State -> Config msg -> List (Html.Attribute msg)
changeEvents state conf =
    -- TODO facets
    [ Events.onBlur (state |> urlEncoder |> conf.toUrl)
    , ExtraEvents.onEnter (state |> urlEncoder |> conf.toUrl)
    ]


view : State -> Config msg -> List (Html msg)
view state conf =
    [ Grid.container []
        (Grid.row
            []
            [ Grid.col []
                [ InputGroup.config
                    (InputGroup.text
                        [ Input.placeholder "SearchComponent for"
                        , Input.attrs (changeEvents state conf)
                        , Input.onInput (\text -> conf.toMsg { state | termSearcher = text })
                        , Input.value state.termSearcher
                        ]
                    )
                    |> InputGroup.successors
                        -- TODO force reload
                        [ InputGroup.button
                            [ Button.primary ]
                            [ text "Go!" ]
                        ]
                    |> InputGroup.view
                ]
            ]
            :: (Array.indexedMap
                    (\i -> renderFieldSearcher conf (\f -> { state | fieldSearchers = Array.set i f state.fieldSearchers }))
                    state.fieldSearchers
                    |> Array.toList
               )
            ++ [ Grid.row []
                    [ Grid.col [ Col.xs1 ]
                        [ Dropdown.dropdown
                            state.newField
                            { options = []
                            , toggleMsg = \d -> conf.toMsg { state | newField = d }
                            , toggleButton = Dropdown.toggle [ Button.primary ] [ text "+" ]
                            , items =
                                map
                                    (\f ->
                                        Dropdown.buttonItem
                                            [ onClick
                                                ({ state
                                                    | fieldSearchers =
                                                        Array.push
                                                            (FieldSearcher Dropdown.initialState f "" Nothing)
                                                            state.fieldSearchers
                                                 }
                                                    |> urlEncoder
                                                    |> conf.toUrl
                                                )
                                            ]
                                            [ text (fieldToString f) ]
                                    )
                                    termFilterableFields
                            }
                        ]
                    ]
               ]
            ++ (state.facetSelectors
                    |> AnyDict.filter (\_ facet -> not (Dict.isEmpty facet) || facetSelected facet)
                    |> AnyDict.toList
                    |> map (renderFacetSearcher state conf)
               )
        )
    ]


selectFacetEntry : State -> Field -> Facet -> String -> FacetEntry -> State
selectFacetEntry state field facet key val =
    { state
        | facetSelectors =
            AnyDict.insert field (Dict.insert key { val | selected = not val.selected } facet) state.facetSelectors
    }


renderFacetSearcher : State -> Config msg -> ( Field, Facet ) -> Html msg
renderFacetSearcher state conf ( field, facet ) =
    Grid.row []
        (List.append
            [ Grid.col [ Col.xsAuto ] [ text (fieldToString field) ]
            , Grid.col [ Col.xs1 ] []
            ]
            (map
                (\( key, val ) ->
                    Grid.col [ Col.xsAuto, Col.textAlign Text.alignXsLeft ]
                        [ (if val.selected then
                            Badge.badgePrimary

                           else
                            Badge.badgeLight
                          )
                            []
                            [ Button.button
                                [ Button.small
                                , Button.onClick (selectFacetEntry state field facet key val |> urlEncoder |> conf.toUrl)
                                ]
                                [ text (key ++ " (" ++ String.fromInt val.count ++ ")") ]
                            ]
                        ]
                )
                (Dict.toList facet)
            )
        )


renderFieldSearcher : Config msg -> (FieldSearcher -> State) -> FieldSearcher -> Html msg
renderFieldSearcher conf stateFromElem fieldSearcher =
    -- TODO
    Grid.row []
        [ Grid.col []
            [ Dropdown.dropdown fieldSearcher.fieldSelect
                { options = []
                , toggleMsg = \d -> { fieldSearcher | fieldSelect = d } |> stateFromElem |> conf.toMsg
                , toggleButton = Dropdown.toggle [ Button.secondary ] [ text (fieldToString fieldSearcher.field) ]
                , items =
                    map
                        (\f ->
                            Dropdown.buttonItem
                                [ { fieldSearcher | field = f, fieldSelect = Dropdown.initialState }
                                    |> stateFromElem
                                    |> urlEncoder
                                    |> conf.toUrl
                                    |> onClick
                                ]
                                [ text (fieldToString f) ]
                        )
                        termFilterableFields
                }
            ]
        , Grid.col []
            [ InputGroup.config
                (InputGroup.text
                    [ Input.placeholder "SearchComponent for"
                    , Input.onInput (\s -> { fieldSearcher | value = s } |> stateFromElem |> conf.toMsg)
                    , Input.attrs (changeEvents (stateFromElem fieldSearcher) conf)
                    , Input.value fieldSearcher.value
                    ]
                )
                |> InputGroup.successors
                    (case fieldSearcher.facetSelect of
                        Nothing ->
                            []

                        Just ( facetDropdownState, facet ) ->
                            [ InputGroup.dropdown facetDropdownState
                                { options = []
                                , toggleMsg = \d -> { fieldSearcher | facetSelect = Just ( d, facet ) } |> stateFromElem |> conf.toMsg
                                , toggleButton =
                                    Dropdown.toggle [ Button.secondary ]
                                        [ text
                                            (let
                                                values =
                                                    facetValues facet
                                             in
                                             if List.length values > 5 then
                                                "..."

                                             else
                                                List.foldl (++) "" values
                                            )
                                        ]
                                , items = []
                                }
                            ]
                    )
                |> InputGroup.view
            ]
        ]
