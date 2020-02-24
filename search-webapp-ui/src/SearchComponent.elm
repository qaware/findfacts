module SearchComponent exposing
    ( Config, State
    , config, empty, encode, decoder, update, merge, subscriptions, view
    , sameQuery, buildFacetQuery
    )

{-| This components controls the search form.


# Types

@docs Config, State


# Component

@docs config, empty, encode, decoder, update, merge, subscriptions, view


# Helpers

@docs sameQuery, buildFacetQuery

-}

import Browser.Events
import DataTypes exposing (..)
import Dict exposing (Dict)
import Dict.Any as AnyDict exposing (AnyDict)
import Html exposing (Html, br, div, text)
import Html.Attributes exposing (attribute)
import Html.Events exposing (onClick)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)
import List.Extra
import Material.Button as Button exposing (buttonConfig)
import Material.Chips as Chip exposing (FilterChip, filterChipConfig)
import Material.Elevation as Elevation
import Material.Icon exposing (iconConfig)
import Material.IconButton as IconButton exposing (iconButtonConfig)
import Material.LayoutGrid as Grid
import Material.List as MList exposing (listConfig, listItemConfig)
import Material.Menu as Menu exposing (menuConfig)
import Material.TextField as TextField exposing (textFieldConfig)
import MaterialExtra
import Maybe.Extra
import Util exposing (anyDictDecoder, consIf, dictDecoder, pairWith, singletonIf, toMaybe)


{-| Opaque config type for the search component.
-}
type Config msg
    = Config (ConfigInternal msg)


type alias ConfigInternal msg =
    { toInternal : State -> msg
    , toMsg : State -> msg
    , exec : FacetQuery -> Int -> msg
    }


{-| Creates a config for the search component.
-}
config : (State -> msg) -> (State -> msg) -> (FacetQuery -> Int -> msg) -> Config msg
config toInternal toMsg exec =
    Config <| ConfigInternal toInternal toMsg exec


{-| Opaque state type for the search component.
-}
type State
    = State StateInternal


type alias StateInternal =
    { filters : List FieldFilter
    , term : String
    , facets : Faceting
    , addMenuState : MenuState
    , fieldSearchers : Dict Int FieldSearcher
    }


type MenuState
    = Opening
    | Open
    | Closed


{-| Allows only one facet per field.
-}
type alias Faceting =
    AnyDict String Field Facet


{-| Value -> (Count, Selected)
-}
type alias Facet =
    Dict String FacetEntry


type alias FacetEntry =
    { count : Maybe Int
    , selected : Bool
    }


type alias FieldSearcher =
    { open : Bool
    , field : Field
    , term : String
    , facet : Maybe Facet
    }


{-| Creates an empty searcher.
-}
empty : State
empty =
    State <| StateInternal [] "" (AnyDict.empty fieldToString) Closed Dict.empty


{-| Encodes the search component persistent parts as JSON.
-}
encode : State -> Value
encode (State state) =
    Encode.object
        ([]
            |> consIf (state.facets |> AnyDict.filter (always facetActive) |> AnyDict.isEmpty >> not)
                ( "facets", encodeFaceting state.facets )
            |> consIf (state.fieldSearchers |> Dict.isEmpty >> not)
                ( "fields", Encode.dict String.fromInt encodeFieldSearcher state.fieldSearchers )
            |> consIf (not (String.isEmpty state.term)) ( "term", Encode.string state.term )
        )


{-| Decodes persistent searcher state (user input!) from json.
-}
decoder : Decoder State
decoder =
    Decode.map State <|
        Decode.map3
            (\term facets fields ->
                StateInternal (buildFQs <| StateInternal [] term facets Closed fields) term facets Closed fields
            )
            (Decode.map (Maybe.withDefault "") <| Decode.maybe <| Decode.field "term" Decode.string)
            (Decode.map (Maybe.withDefault <| AnyDict.empty fieldToString)
                (Decode.maybe <| Decode.field "facets" facetingDecoder)
            )
            (Decode.map
                (Maybe.withDefault Dict.empty)
                (Decode.maybe <|
                    Decode.field "fields" <|
                        dictDecoder (Decode.decodeString Decode.int >> Result.mapError Decode.errorToString)
                            fieldSearcherDecoder
                )
            )


{-| Updates the search component with a new facet result.
-}
update : FacetResult -> Int -> State -> State
update result id (State state) =
    if id < 0 then
        -- no information about selection, but new counts
        State { state | facets = updateFaceting result state.facets }

    else
        State { state | fieldSearchers = Dict.update id (Maybe.map <| updateFieldSearcherFacet result) state.fieldSearchers }


{-| Adds information from previous state that was not encoded in url.
-}
merge : State -> State -> ( State, Maybe (List FieldFilter) )
merge (State oldState) (State newState) =
    let
        facets =
            -- information about selection, but no counts
            mergeFaceting oldState.facets newState.facets

        fieldSearchers =
            mergeFieldSearchers oldState.fieldSearchers newState.fieldSearchers

        outQuery =
            if oldState.filters == newState.filters then
                Nothing

            else
                Just newState.filters
    in
    ( State { newState | filters = newState.filters, facets = facets, fieldSearchers = fieldSearchers }, outQuery )


subscriptions : State -> Config msg -> Sub msg
subscriptions (State state) (Config conf) =
    case state.addMenuState of
        Opening ->
            Browser.Events.onClick (Decode.succeed <| conf.toInternal <| State { state | addMenuState = Open })

        Open ->
            Browser.Events.onClick (Decode.succeed <| conf.toInternal <| State { state | addMenuState = Closed })

        _ ->
            Sub.none


{-| Renders the search component
-}
view : State -> Config msg -> Html msg
view (State state) (Config conf) =
    let
        faceting =
            state.facets
                |> AnyDict.filter (always (Dict.isEmpty >> not))
                |> AnyDict.map (\field -> renderFieldFacet conf (setFieldFacet state field) field)
                |> AnyDict.values
                |> List.intersperse MaterialExtra.divider
    in
    div [] <|
        [ Grid.layoutGrid [ Elevation.z2 ]
            [ TextField.textField
                { textFieldConfig
                    | placeholder = Just "Fuzzy search term with * wildcards"
                    , fullwidth = True
                    , value = state.term
                    , onInput = Just <| setTerm state >> conf.toInternal
                    , onChange = Just <| always <| conf.toMsg (State state)
                }
            ]
        , br [] []
        , Grid.layoutGrid [ Elevation.z2 ]
            ((state.fieldSearchers
                |> Dict.map (renderFieldSearcher conf << setFieldSearcher state)
                |> Dict.values
                |> List.intersperse MaterialExtra.divider
             )
                ++ [ div [ Menu.menuSurfaceAnchor ]
                        [ Button.textButton
                            { buttonConfig
                                | onClick = Just <| conf.toInternal <| State { state | addMenuState = Opening }
                                , icon = Just "add_circle"
                            }
                            "filter"
                        , Menu.menu { menuConfig | open = state.addMenuState == Open }
                            [ fieldSelectionList termFilterableFields
                                (addFieldSearcher state >> conf.toMsg)
                                fieldFacetDisplay
                            ]
                        , br [] []
                        ]
                   ]
            )
        , br [] []
        ]
            ++ singletonIf (not <| List.isEmpty faceting) (Grid.layoutGrid [ Elevation.z2 ] faceting)


{-| Checks if two search components yield the same query.
-}
sameQuery : State -> State -> Bool
sameQuery (State state1) (State state2) =
    state1.filters == state2.filters


{-| Builds a facet query with search facet fields from filters.
-}
buildFacetQuery : List FieldFilter -> FacetQuery
buildFacetQuery fqs =
    FacetQuery fqs facetableFields 10



-- INTERNALS
-- ENCODING


{-| Encode term if nonempty and facet if present and active
-}
encodeFieldSearcher : FieldSearcher -> Encode.Value
encodeFieldSearcher fieldSearcher =
    Encode.object <|
        Maybe.Extra.values <|
            [ Just ( "field", Encode.string (fieldToString fieldSearcher.field) )
            , toMaybe ( "term", Encode.string fieldSearcher.term ) (not <| String.isEmpty fieldSearcher.term)
            , fieldSearcher.facet |> Maybe.andThen (\f -> toMaybe ( "facet", encodeFacet f ) (facetActive f))
            ]


{-| Encode only facets where values are selected
-}
encodeFaceting : Faceting -> Encode.Value
encodeFaceting faceting =
    faceting
        |> AnyDict.toDict
        |> Dict.filter (always facetActive)
        |> Encode.dict identity encodeFacet


{-| Encode only selected values
-}
encodeFacet : Facet -> Encode.Value
encodeFacet facet =
    facet |> facetSelected |> Dict.keys |> Encode.list Encode.string



-- DECODING


facetingDecoder : Decoder Faceting
facetingDecoder =
    anyDictDecoder fieldFromString facetDecoder fieldToString


facetDecoder : Decoder Facet
facetDecoder =
    Decode.map (Dict.fromList << List.map (pairWith (FacetEntry Nothing True))) <|
        Decode.list Decode.string


fieldSearcherDecoder : Decoder FieldSearcher
fieldSearcherDecoder =
    Decode.map3 (FieldSearcher False)
        (Decode.field "field" fieldDecoder)
        (Decode.map (Maybe.withDefault "") <| Decode.maybe <| Decode.field "term" Decode.string)
        (Decode.maybe <| Decode.field "facet" facetDecoder)



-- UPDATING/MERGING


updateFaceting : FacetResult -> Faceting -> Faceting
updateFaceting result faceting =
    let
        -- Inactive facets can be filtered out
        activeFacets =
            AnyDict.filter (always facetActive) faceting

        resultFacets =
            result
                |> AnyDict.map
                    (\field res ->
                        AnyDict.get field activeFacets
                            |> Maybe.map (\facet -> updateFacet res facet)
                            |> Maybe.withDefault (initFacet res)
                    )

        -- Facets that were active, but can't be found in the results any more (likely because there's too many values)
        -- Are marked as stale, i.e. their counts are removed
        staleFacets =
            AnyDict.diff activeFacets resultFacets
                |> AnyDict.map (always <| Dict.map (always <| \entry -> { entry | count = Nothing }))
    in
    -- make sure that
    AnyDict.union resultFacets staleFacets


{-| Updates a facet, keeping facet selection but using new options/counts.
-}
updateFacet : ResultFacet -> Facet -> Facet
updateFacet result facet =
    facetSelectedValues facet |> List.foldl (selectFacetEntry <| FacetEntry (Just 0) True) (initFacet result)


{-| Try to select an entry in a facet. If the entry can't be found, create a default one
-}
selectFacetEntry : FacetEntry -> String -> Facet -> Facet
selectFacetEntry default k facet =
    facet
        |> Dict.update k
            (Maybe.map (\entry -> { entry | selected = True }) >> Maybe.withDefault default >> Just)


updateFieldSearcherFacet : FacetResult -> FieldSearcher -> FieldSearcher
updateFieldSearcherFacet facetResult fieldSearcher =
    case AnyDict.get fieldSearcher.field facetResult of
        Nothing ->
            fieldSearcher

        Just result ->
            { fieldSearcher
                | facet =
                    fieldSearcher.facet
                        |> Maybe.map (updateFacet result)
                        |> Maybe.withDefault (initFacet result)
                        |> Just
            }


{-| Merges an old faceting with all its counts and values with a new one (parsed from url) that only contains selection.
-}
mergeFaceting : Faceting -> Faceting -> Faceting
mergeFaceting info selection =
    AnyDict.merge
        -- No selection: unselect all
        (\field facetInfo -> AnyDict.insert field (mergeFacet facetInfo Dict.empty))
        -- Selection and facet: update accordingly
        (\field facetInfo facetSel -> AnyDict.insert field (mergeFacet facetInfo facetSel))
        -- Selection for a facet that does not yet exist: insert as 'stale' facet (happens at page reload).
        AnyDict.insert
        info
        selection
        (AnyDict.empty fieldToString)


{-| Merges a facet with counts with a facet with selections.
-}
mergeFacet : Facet -> Facet -> Facet
mergeFacet facet selection =
    let
        unselectedFacet =
            Dict.map (always <| \e -> { e | selected = False }) facet
    in
    facetSelectedValues selection |> List.foldl (selectFacetEntry <| FacetEntry Nothing True) unselectedFacet


mergeFieldSearchers : Dict Int FieldSearcher -> Dict Int FieldSearcher -> Dict Int FieldSearcher
mergeFieldSearchers oldFieldSearchers newFieldSearchers =
    Dict.merge
        -- Ignore field searchers that are only in the old state
        (always <| always <| identity)
        -- Update field searchers that are in both
        (\idx old new -> Dict.insert idx (mergeFieldSearcher old new))
        -- Simply use field searchers that are only in new
        Dict.insert
        oldFieldSearchers
        newFieldSearchers
        Dict.empty


mergeFieldSearcher : FieldSearcher -> FieldSearcher -> FieldSearcher
mergeFieldSearcher old new =
    case ( old.facet, new.facet ) of
        ( Nothing, _ ) ->
            new

        ( Just facet, Nothing ) ->
            { new | facet = Just facet }

        ( Just oldFacet, Just newFacet ) ->
            { new | facet = Just <| mergeFacet oldFacet newFacet }



-- STATIC


facetableFields : List Field
facetableFields =
    [ CmdKind, SrcFile, Kind, ConstTypeFacet, DocKind ]


termFilterableFields : List Field
termFilterableFields =
    [ CmdKind, Src, SrcFile, Name, Kind, Prop, ConstType, DocKind ]



-- UTILITIES


facetActive : Facet -> Bool
facetActive facet =
    facet |> Dict.values |> List.any .selected


facetSelected : Facet -> Facet
facetSelected facet =
    facet |> Dict.filter (always .selected)


facetSelectedValues : Facet -> List String
facetSelectedValues f =
    Dict.filter (always .selected) f |> Dict.keys


setFieldFacet : StateInternal -> Field -> Facet -> State
setFieldFacet state field facet =
    State { state | facets = AnyDict.insert field facet state.facets }


setFacetEntry : Facet -> String -> FacetEntry -> Facet
setFacetEntry facet key entry =
    Dict.insert key entry facet


setTerm : StateInternal -> String -> State
setTerm state term =
    State { state | term = term }


setFieldSearcher : StateInternal -> Int -> Maybe FieldSearcher -> State
setFieldSearcher state idx fieldSarcherMaybe =
    case fieldSarcherMaybe of
        Nothing ->
            State { state | fieldSearchers = Dict.remove idx state.fieldSearchers }

        Just fieldSearcher ->
            State { state | fieldSearchers = Dict.insert idx fieldSearcher state.fieldSearchers }


addFieldSearcher : StateInternal -> Field -> State
addFieldSearcher state field =
    let
        newIdx =
            Dict.keys state.fieldSearchers |> List.Extra.last |> Maybe.map ((+) 1) |> Maybe.withDefault 0
    in
    State { state | fieldSearchers = Dict.insert newIdx (FieldSearcher False field "" Nothing) state.fieldSearchers }


initFacet : ResultFacet -> Facet
initFacet queryFacet =
    Dict.map (\_ count -> FacetEntry (Just count) False) queryFacet


initFaceting : FacetResult -> Faceting
initFaceting result =
    AnyDict.map (always initFacet) result



-- QUERYING


buildFQs : StateInternal -> List FieldFilter
buildFQs state =
    Maybe.Extra.values <|
        [ getFilter state.term |> Maybe.map (FieldFilter Src) ]
            ++ (state.fieldSearchers |> Dict.values |> List.map buildFieldSearcherFQ)
            ++ (state.facets
                    |> AnyDict.toList
                    |> List.map (\( field, facet ) -> buildFacetFQ facet |> Maybe.map (FieldFilter field))
               )


getFilter : String -> Maybe Filter
getFilter str =
    if String.isEmpty str then
        Nothing

    else
        Just <|
            if String.startsWith "\"" str && String.endsWith "\"" str then
                str |> String.dropLeft 1 |> String.dropRight 1 |> Exact

            else
                Term str


buildFieldSearcherFQ : FieldSearcher -> Maybe FieldFilter
buildFieldSearcherFQ fieldSearcher =
    Maybe.map (FieldFilter fieldSearcher.field) <|
        case getFilter fieldSearcher.term of
            Nothing ->
                fieldSearcher.facet |> Maybe.andThen buildFacetFQ

            Just term ->
                Just term


buildFacetFQ : Facet -> Maybe Filter
buildFacetFQ facet =
    case facetSelectedValues facet of
        [] ->
            Nothing

        v :: [] ->
            Just <| Exact v

        v1 :: v2 :: vs ->
            Just <| Or (Exact v1) (Exact v2) (vs |> List.map Exact)



-- VIEW


renderFieldFacet : ConfigInternal msg -> (Facet -> State) -> Field -> Facet -> Html msg
renderFieldFacet conf updateFn field facet =
    Grid.layoutGridInner []
        [ Grid.layoutGridCell [ Grid.span2, Grid.alignMiddle ] [ text <| fieldFacetDisplay field ]
        , Grid.layoutGridCell [ Grid.span10 ]
            [ facet
                |> Dict.map (\k -> renderFacetEntry conf (updateFn << setFacetEntry facet k) k)
                |> Dict.values
                |> Chip.filterChipSet []
            ]
        ]


fieldFacetDisplay : Field -> String
fieldFacetDisplay field =
    case field of
        Id ->
            "Id"

        CmdKind ->
            "Command"

        Src ->
            "Source Text"

        SrcFile ->
            "Source Theory"

        Name ->
            "Entity Name"

        Kind ->
            "Entity"

        Prop ->
            "Proposition"

        ConstType ->
            "Type"

        ConstTypeFacet ->
            "Type"

        DocKind ->
            "Documentation"


renderFacetEntry : ConfigInternal msg -> (FacetEntry -> State) -> String -> FacetEntry -> FilterChip msg
renderFacetEntry conf updateFn elem entry =
    Chip.filterChip
        { filterChipConfig
            | selected = entry.selected
            , onClick = Just <| conf.toMsg <| updateFn { entry | selected = not entry.selected }
        }
        (elem ++ (entry.count |> Maybe.map (\c -> " (" ++ String.fromInt c ++ ")") |> Maybe.withDefault ""))


fieldSelectionList : List Field -> (Field -> msg) -> (Field -> String) -> Html msg
fieldSelectionList fields updateFn displayFn =
    MList.list MList.listConfig
        (fields
            |> List.map
                (\field ->
                    MList.listItem
                        { listItemConfig
                            | onClick = Just <| updateFn field
                        }
                        [ text <| displayFn field ]
                )
        )


renderFieldSearcher : ConfigInternal msg -> (Maybe FieldSearcher -> State) -> FieldSearcher -> Html msg
renderFieldSearcher conf stateFromElem fieldSearcher =
    Grid.layoutGridInner []
        [ Grid.layoutGridCell [ Grid.span3Phone, Grid.span7Tablet, Grid.span11Desktop ]
            [ Grid.layoutGridInner []
                [ Grid.layoutGridCell [ Grid.alignMiddle, Grid.span2 ] [ text <| fieldFacetDisplay fieldSearcher.field ]
                , Grid.layoutGridCell [ Grid.alignMiddle, Grid.span9 ] [ renderSelectableTextField conf (Just >> stateFromElem) fieldSearcher ]
                ]
            ]
        , Grid.layoutGridCell [ Grid.span1 ] [ IconButton.iconButton { iconButtonConfig | onClick = Just <| conf.toMsg <| stateFromElem Nothing } "clear" ]
        ]



{- div []
   [ Card.card Card.cardConfig
       { blocks =
           [ Card.cardBlock <|
               Grid.layoutGrid [ Grid.alignLeft ]
                   [ Grid.layoutGridInner
                       []
                       [ Grid.layoutGridCell [ Grid.alignMiddle, Grid.span2 ] [ text <| fieldFacetDisplay fieldSearcher.field ]
                       , Grid.layoutGridCell [] [ renderSelectableTextField conf (Just >> stateFromElem) fieldSearcher ]
                       ]
                   , Grid.layoutGridCell [ Grid.alignMiddle ] [ Chip.inputChipSet [] [ Chip.inputChip Chip.inputChipConfig "..." ] ]
                   ]
           ]
       , actions =
           Just <|
               Card.cardActions
                   { buttons = []
                   , icons =
                       [ Card.cardActionIcon
                           { iconButtonConfig
                               | onClick = Just <| conf.toMsg <| stateFromElem Nothing
                           }
                           "clear"
                       ]
                   }
       }
   , br [] []
   ]
-}


renderSelectableTextField : ConfigInternal msg -> (FieldSearcher -> State) -> FieldSearcher -> Html msg
renderSelectableTextField conf updateFn fieldSearcher =
    case fieldSearcher.facet of
        Nothing ->
            TextField.textField
                { textFieldConfig
                    | value = fieldSearcher.term
                    , onChange = Just <| \text -> conf.toMsg <| updateFn { fieldSearcher | term = text }
                    , onInput = Just <| \text -> conf.toInternal <| updateFn { fieldSearcher | term = text }
                }

        Just facet ->
            Html.div [ Menu.menuSurfaceAnchor ]
                [ TextField.textField
                    { textFieldConfig
                        | leadingIcon =
                            TextField.textFieldIcon
                                { iconConfig
                                    | additionalAttributes =
                                        [ attribute "tabindex" "0"
                                        , attribute "role" "button"
                                        , onClick <| conf.toInternal <| updateFn { fieldSearcher | open = True }
                                        ]
                                }
                                "arrow_drop_down"
                        , value = fieldSearcher.term
                        , onInput = Just <| \text -> conf.toInternal <| updateFn { fieldSearcher | term = text }
                    }
                , Menu.menu
                    { menuConfig
                        | open = fieldSearcher.open
                        , onClose = Just <| conf.toInternal <| updateFn { fieldSearcher | open = False }
                    }
                    [ MList.list
                        { listConfig | wrapFocus = True }
                        (facet
                            |> (\d -> Dict.keys d ++ [ "a", "b" ])
                            |> List.map (text >> List.singleton >> MList.listItem MList.listItemConfig)
                        )
                    ]
                ]
