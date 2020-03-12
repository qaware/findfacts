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
import Html exposing (Html, br, div, h4, text)
import Html.Attributes exposing (style)
import Html.Lazy exposing (lazy)
import Material.Card as Card exposing (cardPrimaryActionConfig)
import Material.DataTable as Table
import Material.Elevation as Elevation
import Material.Extra.Code as Code
import Material.Extra.Divider as Divider
import Material.LayoutGrid as Grid
import Material.LinearProgress as Progress
import Material.Theme as Theme
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
                 , Html.h3 [ Typography.headline6 ] [ text <| "Theory: " ++ stateInternal.block.file ]
                 , div [ Elevation.z2, style "overflow" "auto", style "max-width" "100%", Theme.background ]
                    [ Code.block stateInternal.block.src
                        |> Code.withContext stateInternal.block.srcBefore stateInternal.block.srcAfter
                        |> Code.withLineNumbersFrom stateInternal.block.startLine
                        |> Code.withAdditionalAttrs [ style "display" "inline-block", style "min-width" "100%" ]
                        |> lazy Code.view
                    ]
                 , Html.h2 [ Typography.headline4 ] [ text "Entities" ]
                 ]
                    ++ (stateInternal.block.entities
                            |> List.map (renderEntity stateInternal conf)
                            |> List.intersperse (br [] [])
                       )
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


renderEntity : StateInternal -> ConfigInternal msg -> ShortEt -> Html msg
renderEntity state conf et =
    let
        head =
            Card.cardBlock <|
                Grid.layoutGrid [ Grid.alignLeft ]
                    [ Grid.layoutGridInner []
                        [ Grid.layoutGridCell [ Grid.span1, Typography.body2 ] [ text <| kindToString et.kind ]
                        , Grid.layoutGridCell [ Typography.body1 ] [ text et.name ]
                        ]
                    ]

        body =
            Card.cardBlock <|
                div [] <|
                    (Dict.get et.id state.entityDetails
                        |> Maybe.map (lazy renderEntityDetails >> List.singleton)
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
        , actions = Nothing
        }


renderEntityDetails : EntityState -> Html msg
renderEntityDetails entityState =
    if entityState.open then
        case entityState.resultState of
            Fetching ->
                Progress.indeterminateLinearProgress Progress.linearProgressConfig

            Result res ->
                div []
                    (Divider.divider
                        :: (case res of
                                ThyConstant const ->
                                    [ h4 [ Typography.headline6 ] [ text "Type" ]
                                    , Code.block const.typ
                                        |> Code.withAdditionalAttrs [ style "overflow-x" "auto" ]
                                        |> lazy Code.view
                                    ]
                                        ++ renderUses const.uses

                                ThyFact fact ->
                                    renderUses fact.uses

                                ThyType typ ->
                                    renderUses typ.uses
                           )
                    )

            None ->
                div [] []

    else
        div [] []


renderUses : List ShortEt -> List (Html msg)
renderUses entities =
    h4 [ Typography.headline6 ] [ text "Uses" ]
        :: ite (List.isEmpty entities)
            []
            [ Table.dataTable Table.dataTableConfig
                { thead = []
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
