module Search exposing (..)

import Array exposing (Array)
import Bootstrap.Badge as Badge
import Bootstrap.Button as Button
import Bootstrap.Form.Input as Input
import Bootstrap.Form.InputGroup as InputGroup
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Dict exposing (Dict)
import Dict.Any as AnyDict exposing (AnyDict)
import Html exposing (Html, text)
import List exposing (map)
import Query exposing (AbstractFQ(..), FacetResult, Field(..), FilterTerm(..), Query(..), fieldToString)


type alias State =
    { termSearcher : String
    , fieldSearchers : Array ( Field, String )
    , facetFieldSearchers : AnyDict String Field Facet
    , facetSearchers : AnyDict String Field Facet
    }


type alias Config msg =
    { toMsg : State -> msg
    , exec : msg
    }


type alias Facet =
    Dict String FacetEntry


type alias FacetEntry =
    { count : Int
    , selected : Bool
    }


facetValues : Facet -> List String
facetValues f =
    Dict.filter (\_ entry -> entry.selected) f |> Dict.keys


init : State
init =
    State "" Array.empty (AnyDict.empty fieldToString) (AnyDict.empty fieldToString)


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
                |> Array.map (\( f, s ) -> Filter [ ( f, StringExpression s ) ])
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
                            (List.append (AnyDict.toList state.facetFieldSearchers) (AnyDict.toList state.facetSearchers))
                 in
                 if List.isEmpty singletonSelected then
                    []

                 else
                    [ Filter singletonSelected ]
                )
                (let
                    multipleSelected =
                        List.filter (\( f, facet ) -> List.length (facetValues facet) > 1)
                            (List.append (AnyDict.toList state.facetFieldSearchers) (AnyDict.toList state.facetSearchers))
                 in
                 -- TODO
                 []
                )
    in
    case List.append termFQs (List.append fieldFQs facetFQs) of
        [] ->
            Filter [ ( Query.Id, Query.IdTerm "*" ) ]

        fq :: [] ->
            fq

        fq1 :: fq2 :: fqn ->
            Intersection fq1 fq2 fqn


buildFilterQuery : State -> Query
buildFilterQuery state =
    FilterQuery (buildFQ state) 100


buildFacetQuery : State -> Query
buildFacetQuery state =
    FacetQuery (buildFQ state) Query.facetFields 10



-- UPDATE


update : State -> FacetResult -> State
update state result =
    let
        resultFacetSearchers =
            AnyDict.map (\_ oldFacet -> oldFacet |> Dict.map (\_ count -> FacetEntry count False)) result

        updatedFacetSearchers =
            -- TODO
            state.facetSearchers |> AnyDict.map (\field facet -> facet)
    in
    { state | facetSearchers = AnyDict.union updatedFacetSearchers resultFacetSearchers }



-- VIEW


view : State -> Config msg -> List (Html msg)
view state conf =
    [ Grid.container [] (map (renderFacetSearcher state conf) (AnyDict.toList state.facetSearchers))
    , InputGroup.config
        (InputGroup.text
            (List.append [ Input.placeholder "Search for", Input.onInput (\text -> conf.toMsg { state | termSearcher = text }) ] [])
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


renderFacetSearcher : State -> Config msg -> ( Field, Facet ) -> Html msg
renderFacetSearcher state conf ( field, facet ) =
    Grid.row []
        (List.append
            [ Grid.col [ Col.xs ] [ text (fieldToString field) ]
            , Grid.col [ Col.xs ] []
            ]
            (map
                (\( key, val ) ->
                    Grid.col [ Col.xsAuto ]
                        [ (if val.selected then
                            Badge.badgePrimary

                           else
                            Badge.badgeLight
                          )
                            []
                            [ Button.button
                                [ Button.small
                                , Button.onClick (conf.toMsg { state | facetSearchers = AnyDict.insert field (Dict.insert key { val | selected = not val.selected } facet) state.facetSearchers })
                                ]
                                [ text (key ++ " (" ++ String.fromInt val.count ++ ")") ]
                            ]
                        ]
                )
                (Dict.toList facet)
            )
        )


renderDropdownFacetSearcher : ( Field, List FacetEntry ) -> Html msg
renderDropdownFacetSearcher ( f, facets ) =
    -- TODO
    Grid.row [] []


renderFieldSearcher : ( Field, String ) -> Html msg
renderFieldSearcher ( f, val ) =
    -- TODO
    Grid.row [] []
