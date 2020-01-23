module Search exposing (..)

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
import Query exposing (AbstractFQ(..), FacetResult, Field(..), FilterTerm(..), Query(..), fieldToString, filterableFields)
import Tuple as Pair



-- CONFIG


type alias Config msg =
    { toMsg : State -> msg
    , toBatch : List msg -> msg
    , exec : msg
    }



-- STATE


type alias State =
    { termSearcher : String
    , facetSelectors : Faceting
    , fieldSearchers : Array FieldSearcher
    , newField : Dropdown.State
    }


type alias Facet =
    Dict String FacetEntry


type alias FacetEntry =
    { count : Int
    , selected : Bool
    }


type alias Faceting =
    AnyDict String Field Facet


type alias FieldSearcher =
    { fieldSelect : Dropdown.State
    , field : Field
    , value : String
    , facetSelect : Maybe ( Dropdown.State, Facet )
    }


init : State
init =
    State "" (AnyDict.empty fieldToString) Array.empty Dropdown.initialState



-- UTILITIES


facetValues : Facet -> List String
facetValues f =
    Dict.filter (\_ -> .selected) f |> Dict.keys


facetSelected : Facet -> Bool
facetSelected facet =
    facet |> Dict.values |> List.any .selected



-- QUERYING


buildFacetFQ : Field -> Facet -> Maybe AbstractFQ
buildFacetFQ field facet =
    -- TODO
    Nothing


buildFieldSearcherFQ : FieldSearcher -> Maybe AbstractFQ
buildFieldSearcherFQ fieldSearcher =
    if String.isEmpty fieldSearcher.value then
        fieldSearcher.facetSelect |> Maybe.map Pair.second |> Maybe.andThen (buildFacetFQ fieldSearcher.field)

    else
        Just (Filter [ ( fieldSearcher.field, StringExpression fieldSearcher.value ) ])


buildFQ : State -> AbstractFQ
buildFQ state =
    let
        termFQs =
            if String.isEmpty state.termSearcher then
                []

            else
                [ Union
                    (Filter [ ( Query.Name, StringExpression state.termSearcher ) ])
                    (Filter [ ( Query.Src, StringExpression state.termSearcher ) ])
                    [ Filter [ ( Query.Prop, StringExpression state.termSearcher ) ] ]
                ]

        fieldFQs =
            state.fieldSearchers
                -- TODO
                |> Array.map (\searcher -> Filter [ ( searcher.field, StringExpression "TODO" ) ])
                |> Array.toList

        facetFQs =
            List.append
                (let
                    singletonSelected =
                        List.filterMap
                            (\( f, facet ) ->
                                case facetValues facet of
                                    e :: [] ->
                                        Just ( f, StringExpression e )

                                    _ ->
                                        Nothing
                            )
                            (AnyDict.toList state.facetSelectors)
                 in
                 if List.isEmpty singletonSelected then
                    []

                 else
                    [ Filter singletonSelected ]
                )
                (let
                    multipleSelected =
                        List.filter (\( f, facet ) -> List.length (facetValues facet) > 1)
                            (AnyDict.toList state.facetSelectors)
                 in
                 -- TODO
                 []
                )
    in
    case List.append termFQs (List.append fieldFQs facetFQs) of
        [] ->
            Filter []

        fq :: [] ->
            fq

        fq1 :: fq2 :: fqn ->
            Intersection fq1 fq2 fqn


buildFilterQuery : State -> Query
buildFilterQuery state =
    FilterQuery (buildFQ state) 100


buildFacetQuery : State -> Query
buildFacetQuery state =
    FacetQuery (buildFQ state) Query.facetableFields 10



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


update : State -> FacetResult -> State
update state result =
    { state | facetSelectors = updateFaceting state.facetSelectors result }



-- SUBSCRIPTIONS


subscriptions : State -> (State -> msg) -> Sub msg
subscriptions state toMsg =
    Sub.batch [ Dropdown.subscriptions state.newField (\d -> toMsg { state | newField = d }) ]



-- VIEW


view : State -> Config msg -> List (Html msg)
view state conf =
    [ Grid.container []
        (map (renderFacetSearcher state conf) (AnyDict.toList state.facetSelectors)
            ++ [ Grid.row []
                    [ Grid.col [ Col.xs1 ] []
                    , Grid.col []
                        [ InputGroup.config
                            (InputGroup.text
                                [ Input.placeholder "Search for"
                                , Input.attrs [ Events.onBlur conf.exec, ExtraEvents.onEnter conf.exec ]
                                , Input.onInput (\text -> conf.toMsg { state | termSearcher = text })
                                ]
                            )
                            |> InputGroup.successors
                                [ InputGroup.button
                                    [ Button.primary
                                    , Button.onClick conf.exec
                                    ]
                                    [ text "Go!" ]
                                ]
                            |> InputGroup.view
                        ]
                    ]
               ]
            ++ (Array.indexedMap (\i -> renderFieldSearcher conf (\f -> { state | fieldSearchers = Array.set i f state.fieldSearchers })) state.fieldSearchers |> Array.toList)
            ++ [ Grid.row []
                    [ Grid.col [ Col.xs1 ]
                        [ Dropdown.dropdown
                            state.newField
                            { options = []
                            , toggleMsg = \d -> conf.toMsg { state | newField = d }
                            , toggleButton =
                                Dropdown.toggle
                                    [ Button.primary
                                    , Button.disabled
                                        (state.fieldSearchers
                                            |> Array.toList
                                            |> List.reverse
                                            |> List.head
                                            |> Maybe.map buildFieldSearcherFQ
                                            |> Maybe.map (\_ -> True)
                                            |> Maybe.withDefault False
                                        )
                                    ]
                                    [ text "+" ]
                            , items =
                                map
                                    (\f ->
                                        Dropdown.buttonItem
                                            [ onClick
                                                (conf.toMsg
                                                    { state
                                                        | fieldSearchers =
                                                            Array.push
                                                                (FieldSearcher Dropdown.initialState f "" Nothing)
                                                                state.fieldSearchers
                                                    }
                                                )
                                            ]
                                            [ text (fieldToString f) ]
                                    )
                                    filterableFields
                            }
                        ]
                    ]
               ]
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
                                , Button.onClick
                                    (conf.toBatch
                                        [ conf.toMsg (selectFacetEntry state field facet key val)
                                        , conf.exec
                                        ]
                                    )
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
                                    |> conf.toMsg
                                    |> onClick
                                ]
                                [ text (fieldToString f) ]
                        )
                        filterableFields
                }
            ]
        , Grid.col []
            [ InputGroup.config
                (InputGroup.text
                    [ Input.placeholder "Search for"
                    , Input.onInput (\s -> { fieldSearcher | value = s } |> stateFromElem |> conf.toMsg)
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
