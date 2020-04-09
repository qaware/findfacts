module Components.Details exposing
    ( Config, State
    , config, empty, init, update, view
    )

{-| This component displays blocks in greater detail.


# Types

@docs Config, State


# Component

@docs config, empty, searching, init, update, view

-}

import DataTypes exposing (..)
import Dict exposing (Dict)
import Html exposing (Html, a, div, h4, span, text)
import Html.Attributes exposing (href, style)
import Html.Events exposing (onClick)
import Html.Lazy exposing (lazy, lazy2)
import Material.Button exposing (buttonConfig)
import Material.Card as Card exposing (cardPrimaryActionConfig)
import Material.DataTable as Table exposing (dataTableCellConfig, dataTableConfig, dataTableHeaderCellConfig, dataTableRowConfig)
import Material.Elevation as Elevation
import Material.Extra.Code as Code
import Material.Extra.Divider as Divider
import Material.Extra.Typography as ExtraTypeography
import Material.LayoutGrid as Grid
import Material.LinearProgress as Progress
import Material.Theme as Theme
import Material.Typography as Typography
import Maybe.Extra
import Url.Builder as UrlBuilder
import Util exposing (ite, pairWith, stripTrailingBlankline, trailingBlanklines)


{-| Opaque config type for detail result component.
-}
type Config msg
    = Config (ConfigInternal msg)


type alias ConfigInternal msg =
    { index : String
    , toMsg : Maybe String -> State -> msg
    , toDetailMsg : String -> msg
    , toUsedByMsg : String -> List String -> msg
    , toUsesMsg : String -> List String -> msg
    }


{-| Opaque state type for detail result component.
-}
type State
    = Searching
    | Error String
    | Value StateInternal


type alias StateInternal =
    { block : ShortBlock
    , entityDetails : Dict String EntityState
    }


type alias EntityState =
    { open : Bool
    , resultState : ResultState
    }


{-| State of dynamically loaded data for an entity.
-}
type ResultState
    = None
    | Fetching
    | Result ThyEt


{-| Creates a config for a detail result component.
-}
config :
    String
    -> (Maybe String -> State -> msg)
    -> (String -> msg)
    -> (String -> List String -> msg)
    -> (String -> List String -> msg)
    -> Config msg
config index toMsg toDetailMsg toUsedByMsg toUsesMsg =
    Config (ConfigInternal index toMsg toDetailMsg toUsedByMsg toUsesMsg)


{-| Create an initially empty state.
-}
empty : State
empty =
    Searching


{-| Creates an initial state from detail result.
-}
init : Result String ShortBlock -> State
init result =
    case result of
        Ok block ->
            Value <|
                StateInternal block
                    (block.entities
                        |> List.map (.id >> pairWith (EntityState False None))
                        |> Dict.fromList
                    )

        Err cause ->
            Error cause


{-| Updates the state with a detail result.
-}
update : Result String ThyEt -> State -> State
update result state =
    case ( result, state ) of
        ( Ok res, Value vals ) ->
            let
                id =
                    case res of
                        ThyConstant constantEt ->
                            constantEt.id

                        ThyFact factEt ->
                            factEt.id

                        ThyType typeEt ->
                            typeEt.id
            in
            updateEntityDetail id res vals

        ( Ok _, _ ) ->
            state

        ( Err e, _ ) ->
            Error e


{-| Renders the detail component
-}
view : State -> Config msg -> Html msg
view state (Config conf) =
    case state of
        Error err ->
            text err

        Searching ->
            Progress.indeterminateLinearProgress Progress.linearProgressConfig

        Value stateInternal ->
            div []
                ([ Html.h1 [ Typography.headline3 ] [ text "Details" ]
                 , Html.h3 [ Typography.headline6 ]
                    [ text "Theory: "
                    , a [ href <| UrlBuilder.relative [ "#theory", conf.index, stateInternal.block.file ] [] ]
                        [ text stateInternal.block.file ]
                    ]
                 , div
                    [ Elevation.z2
                    , style "overflow" "auto"
                    , style "max-width" "100%"
                    , style "padding" "16px 0"
                    , Theme.background
                    ]
                    [ Code.block (String.trimRight stateInternal.block.src)
                        |> Code.withContext (stripTrailingBlankline stateInternal.block.srcBefore)
                            ((trailingBlanklines stateInternal.block.src |> List.intersperse "\n" |> String.concat)
                                ++ stateInternal.block.srcAfter
                            )
                        |> Code.withLineNumbersFrom stateInternal.block.startLine
                        |> Code.withAdditionalAttrs [ style "display" "inline-block", style "min-width" "100%" ]
                        |> lazy Code.view
                    ]
                 ]
                    ++ ite (List.isEmpty stateInternal.block.entities)
                        []
                        [ Html.h2 [ Typography.headline4 ] [ text "Entities" ] ]
                    ++ [ Grid.layoutGridInner []
                            (stateInternal.block.entities
                                |> List.sortBy (.kind >> kindCompare)
                                |> List.map (renderEntityCard stateInternal conf >> List.singleton)
                                |> List.map (Grid.layoutGridCell [ Grid.span4Phone, Grid.span8Tablet, Grid.span6Desktop ])
                            )
                       ]
                )



-- INTERNALS
-- UPDATE


toggleOpen : EntityState -> EntityState
toggleOpen es =
    let
        newState =
            case es.resultState of
                None ->
                    Fetching

                _ ->
                    es.resultState
    in
    { es | open = not es.open, resultState = newState }


updateResult : ThyEt -> EntityState -> EntityState
updateResult thyEt es =
    { es | resultState = Result thyEt }


toggleEntityOpen : String -> StateInternal -> State
toggleEntityOpen id state =
    Value { state | entityDetails = Dict.update id (Maybe.map toggleOpen) state.entityDetails }


updateEntityDetail : String -> ThyEt -> StateInternal -> State
updateEntityDetail id thyEt state =
    Value { state | entityDetails = Dict.update id (Maybe.map <| updateResult thyEt) state.entityDetails }


getDetailQuery : String -> Dict String EntityState -> Maybe String
getDetailQuery id states =
    Dict.get id states
        |> Maybe.map
            (\s ->
                case s.resultState of
                    None ->
                        Just id

                    _ ->
                        Nothing
            )
        |> Maybe.Extra.join



-- RENDERING


renderEntityCard : StateInternal -> ConfigInternal msg -> ShortEt -> Html msg
renderEntityCard state conf et =
    let
        head =
            Card.cardBlock <|
                Grid.layoutGrid [ Grid.alignLeft, style "width" "100%" ]
                    [ div [ Typography.caption, style "margin-bottom" "8pt" ] [ text <| kindToString et.kind ]
                    , div [ Typography.body1, style "overflow-wrap" "break-word" ] [ text et.name ]
                    ]

        body =
            Card.cardBlock <|
                div [] <|
                    (Dict.get et.id state.entityDetails
                        |> Maybe.map (lazy2 renderEntityCardInner conf >> List.singleton)
                        |> Maybe.withDefault []
                    )
    in
    Card.card Card.cardConfig
        { blocks =
            Card.cardPrimaryAction
                { cardPrimaryActionConfig
                    | onClick =
                        Just <|
                            conf.toMsg (getDetailQuery et.id state.entityDetails) <|
                                toggleEntityOpen et.id state
                }
                [ head ]
                ++ [ body ]
        , actions = Just <| Card.cardFullBleedActions <| renderFindUsedByButton conf et.name et.id
        }


renderFindUsedByButton conf name id =
    Card.cardActionButton { buttonConfig | onClick = Just <| conf.toUsedByMsg name [ id ] } "used by"


renderEntityCardInner : ConfigInternal msg -> EntityState -> Html msg
renderEntityCardInner conf entityState =
    if entityState.open then
        case entityState.resultState of
            Fetching ->
                Progress.indeterminateLinearProgress Progress.linearProgressConfig

            Result res ->
                div []
                    [ Divider.divider
                    , Grid.layoutGrid [ Grid.alignLeft ]
                        (case res of
                            ThyConstant const ->
                                [ h4 [ Typography.headline6 ] [ text "Type" ]
                                , span [ ExtraTypeography.code1, style "overflow-wrap" "break-word" ] [ text const.typ ]
                                ]
                                    ++ renderUses conf const.uses

                            ThyFact fact ->
                                renderUses conf fact.uses

                            ThyType typ ->
                                renderUses conf typ.uses
                        )
                    ]

            None ->
                div [] []

    else
        div [] []


renderUses : ConfigInternal msg -> List ShortEt -> List (Html msg)
renderUses conf entities =
    h4 [ Typography.headline6 ] [ text "Uses" ]
        :: ite (List.isEmpty entities)
            []
            [ Table.dataTable { dataTableConfig | additionalAttributes = [ style "display" "block" ] }
                { thead =
                    [ Table.dataTableHeaderRow []
                        [ Table.dataTableHeaderCell dataTableHeaderCellConfig [ text "Kind" ]
                        , Table.dataTableHeaderCell dataTableHeaderCellConfig [ text "Name" ]
                        ]
                    ]
                , tbody =
                    entities
                        |> List.map
                            (\et ->
                                Table.dataTableRow
                                    { dataTableRowConfig
                                        | additionalAttributes =
                                            [ onClick <| conf.toDetailMsg et.id
                                            , style "cursor" "pointer"
                                            ]
                                    }
                                    [ Table.dataTableCell
                                        dataTableCellConfig
                                        [ text <| kindToString et.kind ]
                                    , Table.dataTableCell dataTableCellConfig [ text et.name ]
                                    ]
                            )
                }
            ]
