module Components.Search exposing
    ( Config, State, ResultFor(..), MergeResult(..)
    , config, empty, encode, decoder, update, merge, subscriptions, view
    , sameQuery, buildFacetQueries, initUsedBy, initUses, getFilters
    )

{-| This components controls the search form.


# Types

@docs Config, State, ResultFor, MergeResult


# Component

@docs config, empty, encode, decoder, update, merge, subscriptions, view


# Helpers

@docs sameQuery, buildFacetQueries, addUsing, initUsedBy, initUses, getFilters

-}

import Array exposing (Array)
import Array.Extra
import DataTypes exposing (..)
import Dict exposing (Dict)
import Dict.Any as AnyDict exposing (AnyDict)
import Html exposing (Html, br, div, h3, text)
import Html.Attributes exposing (attribute, class, style)
import Html.Events as Events
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Extra as DecodeExtra
import Json.Encode as Encode exposing (Value)
import List.Extra
import Material.Button as Button exposing (buttonConfig)
import Material.Chips as Chips exposing (FilterChip, filterChipConfig, inputChipConfig)
import Material.Elevation as Elevation
import Material.Extra.Code as Code
import Material.Extra.Divider as Divider
import Material.Extra.Menu as Menu
import Material.Icon exposing (iconConfig)
import Material.IconButton as IconButton exposing (iconButtonConfig)
import Material.LayoutGrid as Grid
import Material.List as MList exposing (listItemConfig)
import Material.TextField as TextField exposing (textFieldConfig)
import Material.Theme as Theme
import Material.Typography as Typography
import Maybe.Extra
import Set exposing (Set)
import Util exposing (anyDictDecoder, ite, pairWith, resultStringDecoder, singletonIf, toMaybe)



-- Static configuration


type alias NamedField =
    { field : Field
    , name : String
    }


facetFields : AnyDict String Field NamedField
facetFields =
    AnyDict.fromList fieldToString
        [ ( Command, "Command" )
        , ( SrcFileFacet, "Source Theory" )
        , ( Kind, "Entity Kind" )
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
        [ ( Command, FilterField "Isabelle Command" <| Just Command )
        , ( Src, FilterField "Source Code" Nothing )
        , ( SrcFile, FilterField "Source Theory" <| Just SrcFileFacet )
        , ( Name, FilterField "Semantic Entity Name" <| Just NameFacet )
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
    , facetError : Maybe String
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
    , matchMode : MatchMode
    }


type MatchMode
    = AllOf
    | OneOf
    | Neither


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
            Nothing


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


matchModeToString : MatchMode -> String
matchModeToString mode =
    case mode of
        AllOf ->
            "AllOf"

        OneOf ->
            "OneOf"

        Neither ->
            "Neither"


{-| Encode term if nonempty and facet if present and active
-}
encodeFieldSearcher : FieldSearcher -> Encode.Value
encodeFieldSearcher fieldSearcher =
    Encode.object <|
        Maybe.Extra.values <|
            [ Just ( "field", Encode.string (fieldToString fieldSearcher.field.field) )
            , Just ( "match", Encode.string (matchModeToString fieldSearcher.matchMode) )
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
                    Nothing
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


matchModeFromString : String -> Result String MatchMode
matchModeFromString str =
    case str of
        "OneOf" ->
            Ok OneOf

        "AllOf" ->
            Ok AllOf

        "Neither" ->
            Ok Neither

        _ ->
            Err <| "Match mode " ++ str ++ "does not exist"


fieldSearcherDecoder : Decoder FieldSearcher
fieldSearcherDecoder =
    Decode.map4 (FieldSearcher Menu.closed)
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
        (Decode.field "match" <| resultStringDecoder matchModeFromString)


usageBlockDecoder : Decoder UsageBlock
usageBlockDecoder =
    Decode.map2 UsageBlock
        (Decode.field "block" Decode.string)
        (Decode.field "ids" <| Decode.list Decode.string)


facetingDecoder : Decoder Faceting
facetingDecoder =
    anyDictDecoder
        (fieldFromString
            >> Result.andThen (\f -> AnyDict.get f facetFields |> Result.fromMaybe "Not a facet field")
        )
        facetDecoder
        (.field >> fieldToString)


facetDecoder : Decoder Facet
facetDecoder =
    Decode.map (Dict.fromList << List.map (pairWith (FacetEntry Nothing True))) <|
        Decode.list Decode.string


{-| Specifies which elements a facet result is for.
-}
type ResultFor
    = ForFacets
    | ForFieldSearcher
    | ForFieldSearchers


{-| Updates the search component with a new facet result.
-}
update : Result String ResultFaceting -> ResultFor -> State -> State
update result assignment (State state) =
    case result of
        Ok faceting ->
            case assignment of
                ForFacets ->
                    -- no information about selection, but new counts
                    State { state | facets = updateFaceting faceting state.facets }

                ForFieldSearchers ->
                    State { state | fieldSearcherFacets = faceting }

                ForFieldSearcher ->
                    State { state | fieldSearcherFacets = AnyDict.union state.fieldSearcherFacets faceting }

        Err cause ->
            State { state | facetError = Just cause }


updateFaceting : ResultFaceting -> Faceting -> Faceting
updateFaceting result faceting =
    let
        -- Inactive facets can be filtered out
        activeFacets =
            AnyDict.filter (always facetActive) faceting

        resultFacets =
            result
                |> AnyDict.toList
                |> List.filterMap (\( k, v ) -> AnyDict.get k facetFields |> Maybe.map (pairWith v))
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


{-| Different outcomes of a merge.
-}
type MergeResult
    = -- Return field-filters in case page changed
      UpToDate (List FieldFilter)
    | NewFieldSearcherFacet FacetQuery
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
                    NewFieldSearcherFacet <| FacetQuery newState.filters fsAdditionalFacetFields 100

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
                    | placeholder = Just "Enter search terms with * wildcards..."
                    , label = Just "Source Code"
                    , outlined = True
                    , additionalAttributes = [ style "width" "100%", style "background-color" "white" ]
                    , value = state.term
                    , onInput = Just <| setTerm state >> conf.toInternal
                    , onChange = Just <| always <| conf.toMsg (State state)
                }
            ]
        , br [] []
        , Grid.layoutGrid [ Elevation.z2 ]
            (List.intersperse (Divider.dividerWith [ style "margin" "8px 0" ])
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
                            |> Maybe.map
                                (renderUsage conf (\_ -> State { state | usedIn = Nothing }) "Uses"
                                    >> List.singleton
                                )
                            |> Maybe.withDefault []
                       )
                    ++ (state.uses
                            |> Maybe.map
                                (renderUsage conf (\_ -> State { state | uses = Nothing }) "Used In"
                                    >> List.singleton
                                )
                            |> Maybe.withDefault []
                       )
                )
                ++ [ renderAddSearcherButton conf state ]
            )
        , br [] []
        ]
            ++ (if List.isEmpty faceting then
                    []

                else
                    [ Grid.layoutGrid [ Elevation.z2 ]
                        (Grid.layoutGridCell [] [ h3 [ Typography.headline5 ] [ text "Drill-down Facets" ] ] :: faceting)
                    ]
               )
            ++ (state.facetError
                    |> Maybe.map (text >> List.singleton >> Grid.layoutGrid [ Elevation.z2 ] >> List.singleton)
                    |> Maybe.withDefault []
               )



-- FACET RENDERING


renderFieldFacet : ConfigInternal msg -> (Facet -> State) -> NamedField -> Facet -> Html msg
renderFieldFacet conf updateFn field facet =
    Grid.layoutGridInner [ Typography.body1 ]
        [ Grid.layoutGridCell [ Grid.span2, Grid.alignMiddle ] [ text <| field.name ]
        , Grid.layoutGridCell [ Grid.span10 ]
            [ facet
                |> Dict.map (\k -> renderFacetEntry conf (updateFn << setFacetEntry facet k) k)
                |> Dict.values
                |> Chips.filterChipSet [ class "mdc-chip-set--choice" ]
            ]
        ]


renderFacetEntry : ConfigInternal msg -> (FacetEntry -> State) -> String -> FacetEntry -> FilterChip msg
renderFacetEntry conf updateFn elem entry =
    Chips.filterChip
        { filterChipConfig
            | selected = entry.selected
            , onClick = Just <| conf.toMsg <| updateFn { entry | selected = not entry.selected }
            , additionalAttributes = ite entry.selected [] []
        }
        (elem ++ (entry.count |> Maybe.map (\c -> " (" ++ String.fromInt c ++ ")") |> Maybe.withDefault ""))



-- FIELD SEARCHER RENDERING


renderFieldSearcher : ConfigInternal msg -> ((Maybe FieldSearcher -> State) -> Maybe ResultFacet -> FieldSearcher -> Html msg)
renderFieldSearcher conf updateFn maybeFacet fieldSearcher =
    renderCloseableEntry conf
        (\() -> updateFn Nothing)
        (Grid.layoutGridInner []
            [ Grid.layoutGridCell [ Grid.alignMiddle, Grid.span4Phone, Grid.span8Tablet, Grid.span5Desktop ]
                [ Grid.layoutGridInner []
                    (Grid.layoutGridCell [ Grid.span4Phone, Grid.span5Tablet, Grid.span9Desktop ]
                        [ renderSelectableTextField conf (Just >> updateFn) maybeFacet fieldSearcher ]
                        :: singletonIf (not <| List.isEmpty fieldSearcher.terms)
                            (Grid.layoutGridCell
                                [ Grid.span4Phone, Grid.span2Tablet, Grid.span3Desktop, Grid.alignMiddle ]
                                [ renderSwitchButton conf updateFn fieldSearcher ]
                            )
                    )
                ]
            , Grid.layoutGridCell [ Grid.alignMiddle, Grid.span4Phone, Grid.span8Tablet, Grid.span7Desktop ]
                [ renderFSValues conf (Just >> updateFn) fieldSearcher ]
            ]
        )


renderSwitchButton : ConfigInternal msg -> (Maybe FieldSearcher -> State) -> FieldSearcher -> Html msg
renderSwitchButton conf updateFn fieldSearcher =
    Button.textButton
        { buttonConfig
            | onClick =
                Just { fieldSearcher | matchMode = nextMode fieldSearcher.matchMode }
                    |> updateFn
                    |> conf.toMsg
                    |> Just
        }
        (renderMode fieldSearcher.matchMode)


renderMode : MatchMode -> String
renderMode mode =
    case mode of
        AllOf ->
            "all of"

        OneOf ->
            "one of"

        Neither ->
            "neither"


renderCloseableEntry : ConfigInternal msg -> (() -> State) -> Html msg -> Html msg
renderCloseableEntry conf onClose content =
    Grid.layoutGridInner []
        [ Grid.layoutGridCell [ Grid.span3Phone, Grid.span7Tablet, Grid.span11Desktop ] [ content ]
        , Grid.layoutGridCell [ Grid.span1, Grid.alignTop ]
            [ IconButton.iconButton
                { iconButtonConfig
                    | onClick = Just <| conf.toMsg <| onClose ()
                    , additionalAttributes = [ Theme.primary ]
                }
                "close"
            ]
        ]


renderSelectableTextField : ConfigInternal msg -> ((FieldSearcher -> State) -> Maybe ResultFacet -> FieldSearcher -> Html msg)
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
        , outlined = True
        , additionalAttributes = [ style "width" "100%", style "background-color" "white" ]
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
        , placeholder = Just "Enter phrase to filter for..."
        , label = Just fieldSearcher.field.name
    }


textFieldIcon onClick icon =
    TextField.textFieldIcon
        { iconConfig
            | additionalAttributes =
                [ attribute "tabindex" "0", attribute "role" "button", Events.onClick onClick, Theme.primary ]
        }
        icon


renderFSValues : ConfigInternal msg -> (FieldSearcher -> State) -> FieldSearcher -> Html msg
renderFSValues conf updateFn fieldSearcher =
    Chips.inputChipSet []
        (fieldSearcher.terms
            |> List.indexedMap
                (\i ->
                    Chips.inputChip
                        { inputChipConfig
                            | onTrailingIconClick =
                                Just <|
                                    conf.toMsg <|
                                        updateFn
                                            { fieldSearcher
                                                | terms = List.Extra.removeAt i fieldSearcher.terms
                                            }
                        }
                )
        )



-- USAGE RENDERING


renderUsage : ConfigInternal msg -> (() -> State) -> String -> UsageBlock -> Html msg
renderUsage conf removeFn name usedIn =
    renderCloseableEntry conf
        removeFn
        (Grid.layoutGridInner []
            [ Grid.layoutGridCell [ Typography.body1, Grid.span1Desktop ] [ text name ]
            , Grid.layoutGridCell
                [ Grid.span4Phone
                , Grid.span8Tablet
                , Grid.span11Desktop
                , style "background-color" "white"
                , style "padding-top" "4px"
                , style "padding-left" "4px"
                , style "border-radius" "4px"
                ]
                [ Code.block usedIn.src |> Code.withMaxHeight 160 |> Code.view ]
            ]
        )



-- ADD BUTTON RENDERING


renderAddSearcherButton : ConfigInternal msg -> StateInternal -> Html msg
renderAddSearcherButton conf state =
    Menu.view state.addOpenState
        (Button.textButton
            { buttonConfig
                | onClick = Just (conf.toInternal <| State { state | addOpenState = Menu.open })
                , icon = Just "add_circle"
                , additionalAttributes = [ style "margin" "8px 0" ]
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


{-| Checks if two search components yield the same query.
-}
sameQuery : State -> State -> Bool
sameQuery (State state1) (State state2) =
    buildFQs state1.term state1.fieldSearchers state1.facets state1.usedIn state1.uses
        == buildFQs state2.term state2.fieldSearchers state2.facets state2.usedIn state2.uses


{-| Builds facet queries with search facet fields from filters and field searchers.
-}
buildFacetQueries : State -> List ( FacetQuery, ResultFor )
buildFacetQueries (State state) =
    let
        fsFields =
            fsFacetFields state.fieldSearchers
    in
    ( FacetQuery state.filters (facetFields |> AnyDict.map (always .field) |> AnyDict.values) 10, ForFacets )
        :: ite (List.isEmpty fsFields) [] [ ( FacetQuery state.filters fsFields 100, ForFieldSearchers ) ]


getFilters : State -> List FieldFilter
getFilters (State state) =
    state.filters


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
            Nothing


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
            Nothing



-- UPDATING/MERGING
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
setFieldSearcher state idx fieldSearcherMaybe =
    case fieldSearcherMaybe of
        Nothing ->
            State { state | fieldSearchers = Array.Extra.removeAt idx state.fieldSearchers }

        Just fieldSearcher ->
            State { state | fieldSearchers = Array.set idx fieldSearcher state.fieldSearchers }


addFieldSearcher : StateInternal -> FilterField -> State
addFieldSearcher state field =
    State
        { state
            | fieldSearchers =
                Array.push (FieldSearcher Menu.closed field "" [] OneOf) state.fieldSearchers
        }


initFacet : ResultFacet -> Facet
initFacet queryFacet =
    Dict.map (\_ count -> FacetEntry (Just count) False) queryFacet


fsFacetFields : Array FieldSearcher -> List Field
fsFacetFields fieldSearchers =
    Array.toList fieldSearchers |> List.filterMap (.field >> .facetField) |> List.Extra.uniqueBy fieldToString


nextMode : MatchMode -> MatchMode
nextMode mode =
    case mode of
        OneOf ->
            AllOf

        AllOf ->
            Neither

        Neither ->
            OneOf



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
    let
        filters =
            ite (String.isEmpty fieldSearcher.text)
                []
                [ Exact fieldSearcher.text ]
                ++ (fieldSearcher.terms |> List.map Exact)

        combined =
            case fieldSearcher.matchMode of
                AllOf ->
                    mergeFilters And filters

                OneOf ->
                    mergeFilters Or filters

                Neither ->
                    mergeFilters Or filters |> Maybe.map Not
    in
    Maybe.map (FieldFilter fieldSearcher.field.field) combined


buildUsedInFQ : UsageBlock -> Maybe FieldFilter
buildUsedInFQ usedIn =
    usedIn.ids |> List.map Term |> mergeFilters Or |> Maybe.map (FieldFilter Uses)


buildUsesFQ : UsageBlock -> Maybe FieldFilter
buildUsesFQ uses =
    uses.ids
        |> List.map Term
        |> mergeFilters Or
        |> Maybe.map (FieldFilter Id >> List.singleton >> InResult Uses >> FieldFilter ChildId)


mergeFilters : (Filter -> Filter -> List Filter -> Filter) -> List Filter -> Maybe Filter
mergeFilters combineFn filters =
    case filters of
        [] ->
            Nothing

        f :: [] ->
            Just f

        f1 :: f2 :: fn ->
            Just <| combineFn f1 f2 fn


buildFacetFQ : Facet -> Maybe Filter
buildFacetFQ facet =
    facetSelectedValues facet
        |> List.map Exact
        |> mergeFilters Or
