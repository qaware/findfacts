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
import Html exposing (Html, br, div, pre, text)
import Html.Lazy exposing (lazy)
import Material.Card as Card exposing (cardPrimaryActionConfig)
import Material.DataTable as Table
import Material.Extra.Typography as ExtraTypography
import Material.LayoutGrid as Grid
import Material.LinearProgress as Progress
import Material.Typography as Typography
import Maybe.Extra
import Util exposing (ite, pairWith)


{-| Opaque config type for detail result component.
-}
type Config msg
    = Config (ConfigInternal msg)


type alias ConfigInternal msg =
    { toMsg : Maybe String -> State -> msg }


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


type ResultState
    = None
    | Fetching
    | Result ThyEt


{-| Creates a config for a detail result component.
-}
config : (Maybe String -> State -> msg) -> Config msg
config toMsg =
    Config (ConfigInternal toMsg)


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


view : State -> Config msg -> List (Html msg)
view state (Config conf) =
    case state of
        Error err ->
            [ text err ]

        Searching ->
            [ Progress.indeterminateLinearProgress Progress.linearProgressConfig ]

        Value stateInternal ->
            [ Card.card Card.cardConfig
                { blocks =
                    [ Card.cardBlock <|
                        Grid.layoutGrid
                            [ Grid.alignLeft ]
                            ([ div [ Typography.overline ] [ text stateInternal.block.file ]
                             , br [] []
                             , ExtraTypography.code [] stateInternal.block.src
                             , br [] []
                             ]
                                ++ (stateInternal.block.entities
                                        |> List.map (renderShortEt stateInternal conf)
                                        |> List.intersperse (br [] [])
                                   )
                            )
                    ]
                , actions = Nothing
                }
            ]



-- INTERNALS
-- UPDATE


toggleOpen : EntityState -> EntityState
toggleOpen es =
    { es | open = not es.open }


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


renderShortEt : StateInternal -> ConfigInternal msg -> ShortEt -> Html msg
renderShortEt state conf et =
    Card.card Card.cardConfig
        { blocks =
            Card.cardPrimaryAction
                { cardPrimaryActionConfig
                    | onClick =
                        Just <|
                            conf.toMsg (getDetailQuery et.id state.entityDetails) <|
                                toggleEntityOpen et.id state
                }
                [ Card.cardBlock <|
                    Grid.layoutGrid [ Grid.alignLeft ] <|
                        [ Grid.layoutGridInner []
                            [ Grid.layoutGridCell [] [ text <| kindToString et.kind ]
                            , Grid.layoutGridCell [] [ text et.name ]
                            ]
                        ]
                ]
                ++ (Dict.get et.id state.entityDetails
                        |> Maybe.map (lazy renderDetails >> Card.cardBlock >> List.singleton)
                        |> Maybe.withDefault []
                   )
        , actions = Nothing
        }


renderDetails : EntityState -> Html msg
renderDetails entityState =
    if entityState.open then
        case entityState.resultState of
            Fetching ->
                Progress.indeterminateLinearProgress Progress.linearProgressConfig

            Result res ->
                lazy renderThyEt res

            None ->
                div [] []

    else
        div [] []


renderThyEt : ThyEt -> Html msg
renderThyEt detail =
    case detail of
        ThyConstant const ->
            div [] <|
                [ br [] [], pre [] [ text const.typ ] ]
                    ++ renderEntitiesSummary "Type uses" const.typUses
                    ++ renderEntitiesSummary "Proposition uses" const.propUses

        ThyFact fact ->
            div [] <|
                br [] []
                    :: renderEntitiesSummary "Proposition uses" fact.propUses
                    ++ renderEntitiesSummary "Proof uses" fact.proofUses

        ThyType typ ->
            div [] <|
                br [] []
                    :: renderEntitiesSummary "Type uses" typ.ctorUses


renderEntitiesSummary : String -> List ShortEt -> List (Html msg)
renderEntitiesSummary name entities =
    ite (List.isEmpty entities)
        []
        [ Table.dataTable Table.dataTableConfig
            { thead =
                [ Table.dataTableHeaderRow
                    []
                    [ Table.dataTableHeaderCell Table.dataTableHeaderCellConfig [ text name ] ]
                ]
            , tbody =
                entities
                    |> List.map
                        (.name
                            >> text
                            >> List.singleton
                            >> Table.dataTableCell Table.dataTableCellConfig
                            >> List.singleton
                            >> Table.dataTableRow Table.dataTableRowConfig
                        )
            }
        ]
