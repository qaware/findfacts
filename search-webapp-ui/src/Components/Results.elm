module Components.Results exposing
    ( Config, State
    , config, empty, searching, init, view
    , hasResults
    )

{-| This component displays results.


# Types

@docs Config, State


# Component

@docs config, empty, searching, init, view


# Helpers

@docs nonEmpty

-}

import DataTypes exposing (..)
import Dict exposing (Dict)
import Html exposing (Html, br, div, span, text)
import Html.Attributes exposing (style)
import Html.Lazy exposing (lazy, lazy2)
import Material.Button exposing (buttonConfig)
import Material.Card as Card exposing (cardConfig, cardPrimaryActionConfig)
import Material.DataTable as Table exposing (DataTableRow)
import Material.Elevation as Elevation
import Material.Extra.Code as Code
import Material.LayoutGrid as Grid
import Material.LinearProgress as Progress
import Material.Theme as Theme
import Material.Typography as Typography
import Util exposing (pairWith)


{-| Opaque config type for the results component.
-}
type Config msg
    = Config (ConfigInternal msg)


type alias ConfigInternal msg =
    { toMsg : State -> msg
    , toDetailMsg : String -> msg
    , toUsageMsg : String -> List String -> msg
    }


{-| Opaque state type for the results component.
-}
type State
    = Empty
    | Searching
    | Error String
    | Values StateInternal


type alias StateInternal =
    { blocks : List ShortBlock
    , blockState : Dict String Bool
    }


type alias EntityState =
    { open : Bool
    , detail : Maybe ThyEt
    }


{-| Creates a config for a results component.
-}
config : (State -> msg) -> (String -> msg) -> (String -> List String -> msg) -> Config msg
config toMsg toDetailMsg toUsageMsg =
    Config (ConfigInternal toMsg toDetailMsg toUsageMsg)


{-| Creates an initial empty state for the results component.
-}
empty : State
empty =
    Empty


{-| Creates an initial searching state for the results component.
-}
searching : State
searching =
    Searching


{-| Initializes a new state with the given results. Not an update function since Old results can be discarded.
-}
init : Result String (ResultList ShortBlock) -> State
init result =
    case result of
        Ok resultList ->
            Values
                { blocks = resultList.values
                , blockState =
                    resultList.values
                        |> List.map .id
                        |> List.map (pairWith False)
                        |> Dict.fromList
                }

        Err cause ->
            Error cause


{-| Renders the results component.
-}
view : State -> Config msg -> Html msg
view state (Config conf) =
    div [ Elevation.z2 ] <|
        case state of
            Empty ->
                []

            Searching ->
                [ Progress.indeterminateLinearProgress Progress.linearProgressConfig ]

            Error err ->
                [ text err ]

            Values res ->
                [ Grid.layoutGrid []
                    (res.blocks
                        |> List.map (renderBlock conf res)
                        |> List.intersperse (br [] [])
                    )
                ]


{-| Checks if the results list is filed with results.
-}
hasResults : State -> Bool
hasResults state =
    case state of
        Values _ ->
            True

        _ ->
            False



-- INTERNALS
-- UPDATE


toggleBlockOpen : String -> StateInternal -> State
toggleBlockOpen id state =
    Values { state | blockState = Dict.update id (Maybe.map not) state.blockState }



-- HELPERS
-- RENDERING


renderBlock : ConfigInternal msg -> StateInternal -> ShortBlock -> Html msg
renderBlock conf state block =
    let
        content =
            lazy2 renderBlockContent (state.blockState |> Dict.get block.id |> Maybe.withDefault False) block
    in
    Card.card { cardConfig | additionalAttributes = [ Elevation.z1 ] }
        { blocks =
            Card.cardPrimaryAction
                { cardPrimaryActionConfig | onClick = Just <| conf.toDetailMsg block.id }
                [ Card.cardBlock <| content ]
        , actions =
            if List.isEmpty block.entities then
                Nothing

            else
                Just <|
                    Card.cardActions
                        { buttons =
                            [ block.entities |> List.map .id |> renderFindUsedByButton conf block.src ]
                        , icons = []
                        }
        }


renderFindUsedByButton conf block ids =
    Card.cardActionButton { buttonConfig | onClick = Just <| conf.toUsageMsg block ids } "used by"


renderBlockContent : Bool -> ShortBlock -> Html msg
renderBlockContent open block =
    Grid.layoutGrid [ Grid.alignLeft, style "width" "100%" ]
        [ div [ Typography.caption, style "margin-bottom" "10pt" ] [ text block.file ]
        , Code.block block.src
            |> Code.withLineNumbersFrom block.startLine
            |> Code.view
        , if open then
            Table.dataTable Table.dataTableConfig
                { thead =
                    [ Table.dataTableHeaderRow
                        [ Typography.subtitle2 ]
                        [ Table.dataTableHeaderCell Table.dataTableHeaderCellConfig [ text "Kind" ]
                        , Table.dataTableHeaderCell Table.dataTableHeaderCellConfig [ text "Name" ]
                        ]
                    ]
                , tbody =
                    block.entities
                        |> List.sortBy (.kind >> kindToString)
                        |> List.map renderEntity
                }

          else
            lazy renderEntitySummary block.entities
        ]


renderEntity : ShortEt -> Table.DataTableRow msg
renderEntity et =
    Table.dataTableRow (Table.DataTableRowConfig False [ Typography.body2 ])
        [ Table.dataTableCell Table.dataTableCellConfig [ text <| kindToString et.kind ]
        , Table.dataTableCell Table.dataTableCellConfig [ text et.name ]
        ]


renderEntitySummary : List ShortEt -> Html msg
renderEntitySummary ets =
    ( "Types", List.filter (\et -> et.kind == Type) ets |> List.length )
        :: ( "Constants", List.filter (\et -> et.kind == Constant) ets |> List.length )
        :: [ ( "Facts", List.filter (\et -> et.kind == Fact) ets |> List.length ) ]
        |> List.filter (\( _, c ) -> c > 0)
        |> List.map (\( name, count ) -> renderBadge (name ++ ": " ++ String.fromInt count))
        |> div []


renderBadge : String -> Html msg
renderBadge label =
    span
        [ style "margin-right" "8px"
        , style "padding" "4px 8px"
        , style "border-radius" "16px"
        , Theme.secondaryBg
        , Theme.onSecondary
        , Typography.caption
        ]
        [ text label ]
