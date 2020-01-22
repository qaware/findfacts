module Search exposing (..)

import Array exposing (Array)
import Bootstrap.Badge as Badge
import Bootstrap.Button as Button
import Bootstrap.Form.Input as Input
import Bootstrap.Form.InputGroup as InputGroup
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Bootstrap.Text as Text
import Dict exposing (Dict)
import Dict.Any as AnyDict exposing (AnyDict)
import Html exposing (Html, text)
import List exposing (map)
import Query exposing (AbstractFQ(..), FacetResult, Field(..), FilterTerm(..), Query(..), fieldToString)


type alias State =
    { termSearcher : String
    , fieldSearchers : Array ( Field, String )
    , facetFieldSearchers : Faceting
    , facetSearchers : Faceting
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


type alias Faceting =
    AnyDict String Field Facet


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


facetSelected : Facet -> Bool
facetSelected facet =
    facet |> Dict.values |> List.any (\entry -> entry.selected)


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
                        entry

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
    { state | facetSearchers = updateFaceting state.facetSearchers result }



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
