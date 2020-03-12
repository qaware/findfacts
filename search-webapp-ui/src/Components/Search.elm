module Components.Search exposing
    ( Config, State, ResultFor(..), MergeResult(..)
    , config, empty, encode, decoder, update, merge, subscriptions, view
    , sameQuery, buildFacetQueries, initUsedBy, initUses
    )

{-| This components controls the search form.


# Types

@docs Config, State, ResultFor, MergeResult


# Component

@docs config, empty, encode, decoder, update, merge, subscriptions, view


# Helpers

@docs sameQuery, buildFacetQueries, addUsing, initUsedBy, initUses

-}

import Array exposing (Array)
import Array.Extra
import DataTypes exposing (..)
import Dict exposing (Dict)
import Dict.Any as AnyDict exposing (AnyDict)
import Html exposing (Html, br, div, text)
import Html.Attributes exposing (attribute, class, style)
import Html.Events as Events
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Extra as DecodeExtra
import Json.Encode as Encode exposing (Value)
import List.Extra
import Material.Button as Button exposing (buttonConfig)
import Material.Chips as Chip exposing (FilterChip, filterChipConfig)
import Material.Elevation as Elevation
import Material.Extra.Chips as Chips
import Material.Extra.Code as Code
import Material.Extra.Divider as Divider
import Material.Extra.Menu as Menu
import Material.Icon exposing (iconConfig)
import Material.IconButton as IconButton exposing (iconButtonConfig)
import Material.LayoutGrid as Grid
import Material.List as MList exposing (listItemConfig)
import Material.TextField as TextField exposing (textFieldConfig)
import Material.Typography as Typography
import Maybe.Extra
import Set exposing (Set)
import Util exposing (anyDictDecoder, ite, pairWith, singletonIf, toMaybe)



-- Static configuration


type alias NamedField =
    { field : Field
    , name : String
    }


facetableFields : AnyDict String Field NamedField
facetableFields =
    AnyDict.fromList fieldToString
        [ ( Command, "Command" )
        , ( SrcFile, "Source Theory" )
        , ( Kind, "Entity" )
        , ( ConstTypeFacet, "Type" )
        ]
        |> AnyDict.map NamedField


type alias FilterField =
    { name : String
    , facetField : Maybe Field
    , field : Field
    }


termFilterableFields : AnyDict String Field FilterField
termFilterableFields =
    AnyDict.fromList fieldToString
        [ ( Command, FilterField "Command" <| Just Command )
        , ( Src, FilterField "Source Text" Nothing )
        , ( SrcFile, FilterField "Source Theory" <| Just SrcFile )
        , ( Name, FilterField "Entity Name" <| Just NameFacet )
        , ( ConstType, FilterField "Constant Type" <| Just ConstTypeFacet )
        ]
        |> AnyDict.map (\k v -> v k)


{-| Opaque config type for the search component.
-}
type Config msg
    = Config (ConfigInternal msg)


type alias ConfigInternal msg =
    { toInternal : State -> msg
    , toMsg : State -> msg
    }


{-| Creates a config for the search component.
-}
config : (State -> msg) -> (State -> msg) -> Config msg
config toInternal toMsg =
    Config <| ConfigInternal toInternal toMsg


{-| Opaque state type for the search component.
-}
type State
    = State StateInternal


type alias StateInternal =
    { filters : List FieldFilter
    , addOpenState : Menu.State
    , term : String
    , fieldSearchers : Array FieldSearcher
    , fieldSearcherFacets : ResultFaceting
    , usedIn : Maybe UsageBlock
    , uses : Maybe UsageBlock
    , facets : Faceting
    }


{-| Allows only one facet per field.
-}
type alias Faceting =
    AnyDict String NamedField Facet


{-| Value -> (Count, Selected)
-}
type alias Facet =
    Dict String FacetEntry


type alias FacetEntry =
    { count : Maybe Int
    , selected : Bool
    }


type alias FieldSearcher =
    { selectState : Menu.State
    , field : FilterField
    , text : String
    , terms : List String
    }


type alias UsageBlock =
    { src : String
    , ids : List String
    }


{-| Creates an empty searcher.
-}
empty : State
empty =
    State <|
        StateInternal []
            Menu.closed
            ""
            Array.empty
            (AnyDict.empty fieldToString)
            Nothing
            Nothing
            (AnyDict.empty <| .field >> fieldToString)


{-| Encodes the search component persistent parts as JSON.
-}
encode : State -> Value
encode (State state) =
    Encode.object <|
        Maybe.Extra.values
            [ not (String.isEmpty state.term) |> toMaybe ( "term", Encode.string state.term )
            , (state.fieldSearchers |> Array.isEmpty >> not)
                |> toMaybe ( "fields", Encode.array encodeFieldSearcher state.fieldSearchers )
            , state.usedIn |> Maybe.map (\s -> ( "usedIn", encodeUsageBlock s ))
            , state.uses |> Maybe.map (\s -> ( "uses", encodeUsageBlock s ))
            , (state.facets |> AnyDict.filter (always facetActive) |> AnyDict.isEmpty >> not)
                |> toMaybe ( "facets", encodeFaceting state.facets )
            ]


{-| Decodes persistent searcher state (user input!) from json.
-}
decoder : Decoder State
decoder =
    Decode.map State <|
        Decode.map5
            (\term fields usedIn uses facets ->
                StateInternal (buildFQs term fields facets usedIn uses)
                    Menu.closed
                    term
                    fields
                    (AnyDict.empty fieldToString)
                    usedIn
                    uses
                    facets
            )
            (Decode.map (Maybe.withDefault "") <| DecodeExtra.optionalField "term" Decode.string)
            (Decode.map (Maybe.withDefault Array.empty)
                (DecodeExtra.optionalField "fields" <| Decode.array fieldSearcherDecoder)
            )
            (DecodeExtra.optionalField "usedIn" usageBlockDecoder)
            (DecodeExtra.optionalField "uses" usageBlockDecoder)
            (Decode.map (Maybe.withDefault <| AnyDict.empty <| .field >> fieldToString)
                (DecodeExtra.optionalField "facets" facetingDecoder)
            )


{-| Specifies which elements a facet result is for.
-}
type ResultFor
    = Facets
    | FieldSearchers
    | NewFieldSearcher


{-| Updates the search component with a new facet result.
-}
update : ResultFaceting -> ResultFor -> State -> State
update result assignment (State state) =
    case assignment of
        Facets ->
            -- no information about selection, but new counts
            State { state | facets = updateFaceting result state.facets }

        FieldSearchers ->
            State { state | fieldSearcherFacets = result }

        NewFieldSearcher ->
            State { state | fieldSearcherFacets = AnyDict.union state.fieldSearcherFacets result }


{-| Different outcomes of a merge.
-}
type MergeResult
    = -- Return field-filters in case page changed
      UpToDate (List FieldFilter)
    | NewFacet FacetQuery
    | Outdated (List FieldFilter)


{-| Adds information from previous state that was not encoded in url.
-}
merge : State -> State -> ( State, MergeResult )
merge (State oldState) (State newState) =
    let
        facets =
            -- new state has information about selection, but no counts
            mergeFaceting oldState.facets newState.facets

        fieldSearchers =
            newState.fieldSearchers

        fieldSearcherFacets =
            if oldState.filters == newState.filters then
                oldState.fieldSearcherFacets

            else
                AnyDict.empty fieldToString

        fsAdditionalFacetFields =
            Set.diff
                (newState.fieldSearchers
                    |> fsFacetFields
                    |> List.map fieldToString
                    |> Set.fromList
                )
                (oldState.fieldSearchers
                    |> fsFacetFields
                    |> List.map fieldToString
                    |> Set.fromList
                )
                |> Set.toList
                |> List.map (fieldFromString >> Result.toMaybe)
                |> Maybe.Extra.values

        result =
            if oldState.filters == newState.filters then
                if List.isEmpty fsAdditionalFacetFields then
                    UpToDate newState.filters

                else
                    NewFacet <| FacetQuery newState.filters fsAdditionalFacetFields 100

            else
                Outdated newState.filters
    in
    ( State
        { newState
            | filters = newState.filters
            , facets = facets
            , fieldSearchers = fieldSearchers
            , fieldSearcherFacets = fieldSearcherFacets
        }
    , result
    )


{-| Subscribe to pointer events, in order to close selection menus.
-}
subscriptions : State -> Config msg -> Sub msg
subscriptions (State state) (Config conf) =
    Sub.batch <|
        Menu.subscriptions state.addOpenState (Menu.config (\s -> conf.toInternal <| State { state | addOpenState = s }))
            :: (state.fieldSearchers
                    |> Array.toList
                    |> List.indexedMap
                        (\key searcher ->
                            Menu.subscriptions searcher.selectState
                                (Menu.config
                                    (\s ->
                                        Just { searcher | selectState = s }
                                            |> setFieldSearcher state key
                                            |> conf.toInternal
                                    )
                                    |> Menu.onClose
                                        (\s ->
                                            Just { searcher | selectState = s }
                                                |> setFieldSearcher state key
                                                |> conf.toMsg
                                        )
                                )
                        )
               )


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
                |> List.intersperse Divider.divider
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
            (List.intersperse (Divider.dividerWith [ style "margin-bottom" "8px", style "margin-top" "8px" ])
                ((state.fieldSearchers
                    |> Array.indexedMap
                        (\idx fs ->
                            renderFieldSearcher
                                conf
                                (setFieldSearcher state idx)
                                (fs.field.facetField |> Maybe.andThen (\k -> AnyDict.get k state.fieldSearcherFacets))
                                fs
                        )
                    |> Array.toList
                 )
                    ++ (state.usedIn
                            |> Maybe.map (renderUsedIn conf (\_ -> State { state | usedIn = Nothing }) >> List.singleton)
                            |> Maybe.withDefault []
                       )
                )
                ++ [ renderAddSearcherButton conf state, br [] [] ]
            )
        , br [] []
        ]
            ++ singletonIf (not <| List.isEmpty faceting) (Grid.layoutGrid [ Elevation.z2 ] faceting)


{-| Checks if two search components yield the same query.
-}
sameQuery : State -> State -> Bool
sameQuery (State state1) (State state2) =
    state1.filters == state2.filters


{-| Builds facet queries with search facet fields from filters and field searchers.
-}
buildFacetQueries : State -> List ( FacetQuery, ResultFor )
buildFacetQueries (State state) =
    let
        fsFields =
            fsFacetFields state.fieldSearchers
    in
    ( FacetQuery state.filters (facetableFields |> AnyDict.map (always .field) |> AnyDict.values) 10, Facets )
        :: ite (List.isEmpty fsFields) [] [ ( FacetQuery state.filters fsFields 100, FieldSearchers ) ]


initUsedBy : String -> List String -> State
initUsedBy block ids =
    State <|
        StateInternal []
            Menu.closed
            ""
            Array.empty
            (AnyDict.empty fieldToString)
            (Just <| UsageBlock block ids)
            Nothing
            (AnyDict.empty (.field >> fieldToString))


initUses : String -> List String -> State
initUses block ids =
    State <|
        StateInternal []
            Menu.closed
            ""
            Array.empty
            (AnyDict.empty fieldToString)
            Nothing
            (Just <| UsageBlock block ids)
            (AnyDict.empty (.field >> fieldToString))



-- INTERNALS
-- ENCODING


{-| Encode term if nonempty and facet if present and active
-}
encodeFieldSearcher : FieldSearcher -> Encode.Value
encodeFieldSearcher fieldSearcher =
    Encode.object <|
        Maybe.Extra.values <|
            [ Just ( "field", Encode.string (fieldToString fieldSearcher.field.field) )
            , toMaybe ( "text", Encode.string fieldSearcher.text ) (not <| String.isEmpty fieldSearcher.text)
            , toMaybe ( "terms", Encode.list Encode.string fieldSearcher.terms )
                (not <| List.isEmpty fieldSearcher.terms)
            ]


encodeUsageBlock : UsageBlock -> Encode.Value
encodeUsageBlock usageBlock =
    Encode.object
        [ ( "block", usageBlock.src |> Encode.string )
        , ( "ids", usageBlock.ids |> Encode.list Encode.string )
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


fieldSearcherDecoder : Decoder FieldSearcher
fieldSearcherDecoder =
    Decode.map3 (FieldSearcher Menu.closed)
        (Decode.field "field" fieldDecoder
            |> Decode.andThen
                (\f ->
                    AnyDict.get f termFilterableFields
                        |> Maybe.map Decode.succeed
                        |> Maybe.withDefault (Decode.fail "Not a filterable field")
                )
        )
        (DecodeExtra.optionalField "text" Decode.string |> Decode.map (Maybe.withDefault ""))
        (Decode.list Decode.string |> DecodeExtra.optionalField "terms" |> Decode.map (Maybe.withDefault []))


usageBlockDecoder : Decoder UsageBlock
usageBlockDecoder =
    Decode.map2 UsageBlock
        (Decode.field "block" Decode.string)
        (Decode.field "ids" <| Decode.list Decode.string)


facetingDecoder : Decoder Faceting
facetingDecoder =
    anyDictDecoder
        (fieldFromString
            >> Result.andThen (\f -> AnyDict.get f facetableFields |> Result.fromMaybe "Not a facet field")
        )
        facetDecoder
        (.field >> fieldToString)


facetDecoder : Decoder Facet
facetDecoder =
    Decode.map (Dict.fromList << List.map (pairWith (FacetEntry Nothing True))) <|
        Decode.list Decode.string



-- UPDATING/MERGING


updateFaceting : ResultFaceting -> Faceting -> Faceting
updateFaceting result faceting =
    let
        -- Inactive facets can be filtered out
        activeFacets =
            AnyDict.filter (always facetActive) faceting

        resultFacets =
            result
                |> AnyDict.toList
                |> List.filterMap (\( k, v ) -> AnyDict.get k facetableFields |> Maybe.map (pairWith v))
                |> AnyDict.fromList (.field >> fieldToString)
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
        (AnyDict.empty (.field >> fieldToString))


{-| Merges a facet with counts with a facet with selections.
-}
mergeFacet : Facet -> Facet -> Facet
mergeFacet facet selection =
    let
        unselectedFacet =
            Dict.map (always <| \e -> { e | selected = False }) facet
    in
    facetSelectedValues selection |> List.foldl (selectFacetEntry <| FacetEntry Nothing True) unselectedFacet



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


setFieldFacet : StateInternal -> NamedField -> Facet -> State
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
            State { state | fieldSearchers = Array.Extra.removeAt idx state.fieldSearchers }

        Just fieldSearcher ->
            State { state | fieldSearchers = Array.set idx fieldSearcher state.fieldSearchers }


addFieldSearcher : StateInternal -> FilterField -> State
addFieldSearcher state field =
    State
        { state
            | fieldSearchers =
                Array.push (FieldSearcher Menu.closed field "" []) state.fieldSearchers
        }


initFacet : ResultFacet -> Facet
initFacet queryFacet =
    Dict.map (\_ count -> FacetEntry (Just count) False) queryFacet


fsFacetFields : Array FieldSearcher -> List Field
fsFacetFields fieldSearchers =
    Array.toList fieldSearchers |> List.filterMap (.field >> .facetField) |> List.Extra.uniqueBy fieldToString



-- QUERYING


buildFQs : String -> Array FieldSearcher -> Faceting -> Maybe UsageBlock -> Maybe UsageBlock -> List FieldFilter
buildFQs term fieldSearchers facets usedIn uses =
    Maybe.Extra.values <|
        [ getFilter term |> Maybe.map (FieldFilter Src) ]
            ++ (fieldSearchers |> Array.toList |> List.map buildFieldSearcherFQ)
            ++ (facets
                    |> AnyDict.toList
                    |> List.map (\( field, facet ) -> buildFacetFQ facet |> Maybe.map (FieldFilter field.field))
               )
            ++ (usedIn |> Maybe.map (buildUsedInFQ >> List.singleton) |> Maybe.withDefault [])
            ++ (uses |> Maybe.map (buildUsesFQ >> List.singleton) |> Maybe.withDefault [])


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
    ite (String.isEmpty fieldSearcher.text) [] [ Exact fieldSearcher.text ]
        ++ (fieldSearcher.terms |> List.map Exact)
        |> unionFilters
        |> Maybe.map (FieldFilter fieldSearcher.field.field)


buildUsedInFQ : UsageBlock -> Maybe FieldFilter
buildUsedInFQ usedIn =
    usedIn.ids |> List.map Term |> unionFilters |> Maybe.map (FieldFilter Uses)


buildUsesFQ : UsageBlock -> Maybe FieldFilter
buildUsesFQ uses =
    uses.ids
        |> List.map Term
        |> unionFilters
        |> Maybe.map (FieldFilter Id >> List.singleton >> InResult Uses >> FieldFilter Id)


unionFilters : List Filter -> Maybe Filter
unionFilters filters =
    case filters of
        [] ->
            Nothing

        f :: [] ->
            Just f

        f1 :: f2 :: fn ->
            Just <| Or f1 f2 fn


buildFacetFQ : Facet -> Maybe Filter
buildFacetFQ facet =
    facetSelectedValues facet
        |> List.map Exact
        |> unionFilters



-- VIEW


renderFieldSearcher :
    ConfigInternal msg
    -> ((Maybe FieldSearcher -> State) -> Maybe ResultFacet -> FieldSearcher -> Html msg)
renderFieldSearcher conf updateFn maybeFacet fieldSearcher =
    renderCloseableEntry conf
        (\() -> updateFn Nothing)
        fieldSearcher.field.name
        (Grid.layoutGridInner []
            [ Grid.layoutGridCell [ Grid.alignMiddle, Grid.span4Phone, Grid.span8Tablet, Grid.span4Desktop ] [ renderSelectableTextField conf (Just >> updateFn) maybeFacet fieldSearcher ]
            , Grid.layoutGridCell [ Grid.alignMiddle, Grid.span8 ] [ renderFSValues conf (Just >> updateFn) fieldSearcher ]
            ]
        )


renderCloseableEntry : ConfigInternal msg -> (() -> State) -> String -> Html msg -> Html msg
renderCloseableEntry conf onClose name content =
    Grid.layoutGridInner []
        [ Grid.layoutGridCell [ Grid.span3Phone, Grid.span7Tablet, Grid.span11Desktop ]
            [ Grid.layoutGridInner [ Typography.body1 ]
                [ Grid.layoutGridCell [ Grid.alignTop, Grid.span4Phone, Grid.span3Tablet, Grid.span2Desktop ] [ div [ style "padding-top" "16px" ] [ text name ] ]
                , Grid.layoutGridCell [ Grid.alignMiddle, Grid.span5Tablet, Grid.span10Desktop ] [ content ]
                ]
            ]
        , Grid.layoutGridCell [ Grid.span1, Grid.alignTop ]
            [ IconButton.iconButton { iconButtonConfig | onClick = Just <| conf.toMsg <| onClose () } "close" ]
        ]


renderSelectableTextField :
    ConfigInternal msg
    -> ((FieldSearcher -> State) -> Maybe ResultFacet -> FieldSearcher -> Html msg)
renderSelectableTextField conf updateFn maybeFacet fieldSearcher =
    let
        textFieldConf =
            fsTextFieldConfig conf updateFn fieldSearcher
    in
    case maybeFacet of
        Nothing ->
            TextField.textField textFieldConf

        Just facet ->
            let
                remainingValues =
                    facet |> Dict.filter (\term -> always <| List.Extra.notMember term fieldSearcher.terms)
            in
            if Dict.isEmpty remainingValues then
                TextField.textField textFieldConf

            else
                let
                    toMsg =
                        if Dict.size remainingValues == 1 then
                            conf.toMsg

                        else
                            conf.toInternal
                in
                Menu.view fieldSearcher.selectState
                    (TextField.textField
                        { textFieldConf
                            | leadingIcon =
                                textFieldIcon
                                    (conf.toInternal <| updateFn { fieldSearcher | selectState = Menu.open })
                                    "arrow_drop_down"
                        }
                    )
                    (Dict.map
                        (\term count ->
                            MList.listItem
                                { listItemConfig
                                    | onClick =
                                        Just <|
                                            toMsg <|
                                                updateFn { fieldSearcher | terms = term :: fieldSearcher.terms }
                                }
                                [ text <| term ++ " (" ++ String.fromInt count ++ ")" ]
                        )
                        remainingValues
                        |> Dict.values
                    )


fsTextFieldConfig : ConfigInternal msg -> (FieldSearcher -> State) -> FieldSearcher -> TextField.TextFieldConfig msg
fsTextFieldConfig conf updateFn fieldSearcher =
    { textFieldConfig
        | value = fieldSearcher.text
        , fullwidth = False
        , outlined = False
        , trailingIcon =
            textFieldIcon
                (conf.toMsg <|
                    updateFn
                        (if String.isEmpty <| String.trim fieldSearcher.text then
                            fieldSearcher

                         else
                            { fieldSearcher
                                | text = ""
                                , terms = String.trim fieldSearcher.text :: fieldSearcher.terms
                            }
                        )
                )
                "add"
        , onInput = Just <| \text -> conf.toInternal <| updateFn { fieldSearcher | text = text }
        , onChange =
            Just <|
                always <|
                    conf.toMsg <|
                        updateFn
                            { fieldSearcher
                                | text = ""
                                , terms = String.trim fieldSearcher.text :: fieldSearcher.terms
                            }
        , placeholder = Just "Exact search term"
    }


textFieldIcon onClick icon =
    TextField.textFieldIcon
        { iconConfig
            | additionalAttributes =
                [ attribute "tabindex" "0", attribute "role" "button", Events.onClick onClick ]
        }
        icon


renderFSValues : ConfigInternal msg -> (FieldSearcher -> State) -> FieldSearcher -> Html msg
renderFSValues conf updateFn fieldSearcher =
    Chips.view
        (Chips.config (\terms -> conf.toMsg <| updateFn { fieldSearcher | terms = terms }))
        (Chips.inputSet (fieldSearcher.terms |> List.map Chips.input))


renderAddSearcherButton : ConfigInternal msg -> StateInternal -> Html msg
renderAddSearcherButton conf state =
    Menu.view state.addOpenState
        (Button.textButton
            { buttonConfig
                | onClick = Just (conf.toInternal <| State { state | addOpenState = Menu.open })
                , icon = Just "add_circle"
            }
            "filter"
        )
        (termFilterableFields
            |> AnyDict.values
            |> List.map
                (\filterField ->
                    MList.listItem
                        { listItemConfig | onClick = Just <| conf.toMsg <| addFieldSearcher state filterField }
                        [ text <| filterField.name ]
                )
        )


renderUsedIn : ConfigInternal msg -> (() -> State) -> UsageBlock -> Html msg
renderUsedIn conf removeFn usedIn =
    renderCloseableEntry conf
        removeFn
        "Uses"
        (Code.block usedIn.src |> Code.withMaxHeight 160 |> Code.view)


renderFieldFacet : ConfigInternal msg -> (Facet -> State) -> NamedField -> Facet -> Html msg
renderFieldFacet conf updateFn field facet =
    Grid.layoutGridInner [ Typography.body1 ]
        [ Grid.layoutGridCell [ Grid.span2, Grid.alignMiddle ] [ text <| field.name ]
        , Grid.layoutGridCell [ Grid.span10 ]
            [ facet
                |> Dict.map (\k -> renderFacetEntry conf (updateFn << setFacetEntry facet k) k)
                |> Dict.values
                |> Chip.filterChipSet [ class "mdc-chip-set--choice" ]
            ]
        ]


renderFacetEntry : ConfigInternal msg -> (FacetEntry -> State) -> String -> FacetEntry -> FilterChip msg
renderFacetEntry conf updateFn elem entry =
    Chip.filterChip
        { filterChipConfig
            | selected = entry.selected
            , onClick = Just <| conf.toMsg <| updateFn { entry | selected = not entry.selected }
            , additionalAttributes = ite entry.selected [] []
        }
        (elem ++ (entry.count |> Maybe.map (\c -> " (" ++ String.fromInt c ++ ")") |> Maybe.withDefault ""))
