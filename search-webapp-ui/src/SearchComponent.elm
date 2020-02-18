module SearchComponent exposing (Config, State, decoder, encode, init, subscriptions, update, updateWithResult, view)

import Array exposing (Array)
import Bootstrap.Badge as Badge
import Bootstrap.Button as Button
import Bootstrap.Dropdown as Dropdown
import Bootstrap.Form.Input as Input
import Bootstrap.Form.InputGroup as InputGroup
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Bootstrap.Text as Text
import DataTypes exposing (..)
import Dict exposing (Dict)
import Dict.Any as AnyDict exposing (AnyDict)
import Html exposing (Html, div, text)
import Html.Events as Events exposing (onClick)
import Html.Events.Extra as ExtraEvents
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)
import Maybe.Extra
import Result.Extra
import Tuple as Pair
import Util exposing (anyDictDecoder, consIf)



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
    { toInternal : State -> msg
    , toMsg : State -> msg
    , exec : FacetQuery -> Int -> msg
    }



-- STATE


{-| All state that needs to be kept track of
-}
type alias State =
    { newField : Dropdown.State
    , lastQuery : Maybe AbstractFQ
    , termSearcher : String
    , facetSelectors : Faceting
    , fieldSearchers : Array FieldSearcher
    }


type alias FacetSelection =
    -- Name -> FacetEntry
    Dict String FacetEntry


type alias FacetEntry =
    { count : Int
    , selected : Bool
    }


type alias Faceting =
    -- Field -> (implicit dict key) -> Facet Values
    AnyDict String Field FacetSelection


type alias FieldSearcher =
    { fieldSelect : Dropdown.State
    , field : Field
    , value : String
    , facetSelect : Maybe ( Dropdown.State, FacetSelection )
    }


init : State
init =
    State Dropdown.initialState Nothing "" (AnyDict.empty fieldToString) Array.empty



-- UTILITIES


facetValues : FacetSelection -> List String
facetValues f =
    Dict.filter (\_ -> .selected) f |> Dict.keys


facetSelected : FacetSelection -> Bool
facetSelected facet =
    facet |> Dict.values |> List.any .selected


getFilterTerm : String -> FilterTerm
getFilterTerm str =
    if String.startsWith "\"" str && String.endsWith "\"" str then
        str |> String.dropLeft 1 |> String.dropRight 1 |> Exact

    else
        Term str



-- ENCODING


encodeFacetEntry : FacetEntry -> Encode.Value
encodeFacetEntry facetEntry =
    Encode.object [ ( "count", Encode.int facetEntry.count ), ( "selected", Encode.bool facetEntry.selected ) ]


encodeFacet : FacetSelection -> Encode.Value
encodeFacet facet =
    Encode.dict identity encodeFacetEntry facet


encodeFaceting : Faceting -> Encode.Value
encodeFaceting faceting =
    faceting |> AnyDict.toDict |> Encode.dict identity encodeFacet


encodeFieldSearcher : FieldSearcher -> Encode.Value
encodeFieldSearcher fieldSearcher =
    Encode.object
        ([ Just ( "field", Encode.string (fieldToString fieldSearcher.field) )
         , Just ( "value", Encode.string fieldSearcher.value )
         , fieldSearcher.facetSelect |> Maybe.map (\( _, facet ) -> ( "facetSelect", encodeFacet facet ))
         ]
            |> Maybe.Extra.values
        )


encode : State -> Value
encode state =
    Encode.object
        ([ ( "facets", encodeFaceting state.facetSelectors )
         , ( "fields", Encode.array encodeFieldSearcher state.fieldSearchers )
         ]
            |> consIf (not (String.isEmpty state.termSearcher)) ( "term", Encode.string state.termSearcher )
        )



-- DECODING


facetEntryDecoder : Decoder FacetEntry
facetEntryDecoder =
    Decode.map2 FacetEntry (Decode.field "count" Decode.int) (Decode.field "selected" Decode.bool)


facetSelectionDecoder : Decoder FacetSelection
facetSelectionDecoder =
    Decode.dict facetEntryDecoder


facetingDecoder : Decoder Faceting
facetingDecoder =
    anyDictDecoder fieldFromString facetSelectionDecoder fieldToString


fieldSearchersDecoder : Decoder FieldSearcher
fieldSearchersDecoder =
    Decode.map3 (FieldSearcher Dropdown.initialState)
        (Decode.field "field" fieldDecoder)
        (Decode.field "value" Decode.string)
        (Decode.field "facetSelect" facetSelectionDecoder |> Decode.map (Tuple.pair Dropdown.initialState) |> Decode.maybe)


decoder : Decoder State
decoder =
    Decode.map3 (State Dropdown.initialState Nothing)
        (Decode.field "term" Decode.string |> Decode.maybe |> Decode.map (Maybe.withDefault ""))
        (Decode.field "facets" facetingDecoder)
        (Decode.field "fields" (Decode.array fieldSearchersDecoder))



-- QUERYING


buildSingleFQ : Field -> FilterTerm -> AbstractFQ
buildSingleFQ field term =
    Filter [ ( field, term ) ]


buildFacetFQ : Field -> FacetSelection -> Maybe AbstractFQ
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
                        |> List.map Exact
                        |> List.map (buildSingleFQ field)
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
                [ Union (buildSingleFQ Name filterTerm)
                    (buildSingleFQ Src filterTerm)
                    [ buildSingleFQ Prop filterTerm ]
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


makeFacet : Facet -> FacetSelection
makeFacet queryFacet =
    Dict.map (\_ count -> FacetEntry count False) queryFacet


updateCounts : FacetSelection -> Facet -> FacetSelection
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


updateFacet : FacetSelection -> Maybe Facet -> FacetSelection
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


updateWithResult : State -> FacetResult -> Int -> State
updateWithResult state result id =
    { state | facetSelectors = updateFaceting state.facetSelectors result }


update : State -> ( State, AbstractFQ )
update state =
    let
        fq =
            buildFQ state
    in
    ( { state | lastQuery = Just fq }, fq )



-- SUBSCRIPTIONS


subscriptions : State -> (State -> msg) -> Sub msg
subscriptions state toMsg =
    Sub.batch [ Dropdown.subscriptions state.newField (\d -> toMsg { state | newField = d }) ]



-- VIEW


changeEvents : State -> Config msg -> List (Html.Attribute msg)
changeEvents state conf =
    -- TODO facets
    [ Events.onBlur (state |> conf.toMsg)
    , ExtraEvents.onEnter (state |> conf.toMsg)
    ]


view : State -> Config msg -> Html msg
view state conf =
    div [] <|
        Grid.row
            []
            [ Grid.col []
                [ InputGroup.config
                    (InputGroup.text
                        [ Input.placeholder "SearchComponent for"
                        , Input.attrs (changeEvents state conf)
                        , Input.onInput (\text -> conf.toInternal { state | termSearcher = text })
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
                            , toggleMsg = \toggle -> conf.toInternal { state | newField = toggle }
                            , toggleButton = Dropdown.toggle [ Button.primary ] [ text "+" ]
                            , items =
                                List.map
                                    (\f ->
                                        Dropdown.buttonItem
                                            [ onClick
                                                ({ state
                                                    | fieldSearchers =
                                                        Array.push
                                                            (FieldSearcher Dropdown.initialState f "" Nothing)
                                                            state.fieldSearchers
                                                 }
                                                    |> conf.toMsg
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
                    |> List.map (renderFacetSearcher state conf)
               )


selectFacetEntry : State -> Field -> FacetSelection -> String -> FacetEntry -> State
selectFacetEntry state field facet key val =
    { state
        | facetSelectors =
            AnyDict.insert field (Dict.insert key { val | selected = not val.selected } facet) state.facetSelectors
    }


renderFacetSearcher : State -> Config msg -> ( Field, FacetSelection ) -> Html msg
renderFacetSearcher state conf ( field, facet ) =
    Grid.row []
        (List.append
            [ Grid.col [ Col.xsAuto ] [ text (fieldToString field) ]
            , Grid.col [ Col.xs1 ] []
            ]
            (List.map
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
                                , Button.onClick (selectFacetEntry state field facet key val |> conf.toMsg)
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
                , toggleMsg = \toggle -> { fieldSearcher | fieldSelect = toggle } |> stateFromElem |> conf.toInternal
                , toggleButton = Dropdown.toggle [ Button.secondary ] [ text (fieldToString fieldSearcher.field) ]
                , items =
                    List.map
                        (\f ->
                            Dropdown.buttonItem
                                [ { fieldSearcher | field = f, fieldSelect = Dropdown.initialState }
                                    |> stateFromElem
                                    |> conf.toMsg
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
                    , Input.onInput (\s -> { fieldSearcher | value = s } |> stateFromElem |> conf.toInternal)
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
                                , toggleMsg =
                                    \toggle ->
                                        { fieldSearcher | facetSelect = Just ( toggle, facet ) }
                                            |> stateFromElem
                                            |> conf.toInternal
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
