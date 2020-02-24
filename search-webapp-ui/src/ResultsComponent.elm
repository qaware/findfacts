module ResultsComponent exposing
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

import Bootstrap.Badge as Badge
import Bootstrap.Spinner as Spinner
import DataTypes exposing (..)
import Dict exposing (Dict)
import Html exposing (Html, br, div, pre, text)
import Html.Attributes exposing (style)
import Html.Lazy exposing (lazy, lazy2)
import Material.Button exposing (buttonConfig)
import Material.Card as Card exposing (cardConfig, cardPrimaryActionConfig)
import Material.DataTable as Table exposing (DataTableRow)
import Material.Elevation as Elevation
import Material.LayoutGrid as Grid
import Material.Typography as Typography
import Util exposing (pairWith, renderHtml)


{-| Opaque config type for the results component.
-}
type Config msg
    = Config (ConfigInternal msg)


type alias ConfigInternal msg =
    { toMsg : State -> msg
    , toDetailMsg : String -> msg
    , toUsageMsg : List String -> msg
    }


{-| Opaque state type for the results component.
-}
type State
    = Empty
    | Searching
    | Error String
    | Values StateInternal


type alias StateInternal =
    { blocks : List ShortCmd
    , blockState : Dict String Bool
    }


type alias EntityState =
    { open : Bool
    , detail : Maybe ThyEt
    }


{-| Creates a config for a results component.
-}
config : (State -> msg) -> (String -> msg) -> (List String -> msg) -> Config msg
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
init : Result String (ResultList ShortCmd) -> State
init result =
    case result of
        Ok resultList ->
            Values
                { blocks = resultList.values
                , blockState =
                    resultList.values
                        |> getBlocks
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
                [ Spinner.spinner [] [] ]

            Error err ->
                [ text err ]

            Values res ->
                [ Grid.layoutGrid []
                    (res.blocks
                        |> List.map (renderCmd conf res)
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


getBlocks cmds =
    cmds
        |> List.filterMap
            (\cmd ->
                case cmd of
                    Block block ->
                        Just block

                    _ ->
                        Nothing
            )



-- RENDERING


renderCmd : ConfigInternal msg -> StateInternal -> ShortCmd -> Html msg
renderCmd conf state cmd =
    case cmd of
        Doc doc ->
            lazy renderDocItem (renderDocContent doc)

        Block block ->
            renderBlockItem conf state block


renderDocItem : Html msg -> Html msg
renderDocItem content =
    Card.card { cardConfig | additionalAttributes = [ Elevation.z1 ] }
        { blocks = [ Card.cardBlock <| Grid.layoutGrid [ Grid.alignLeft ] [ content ] ], actions = Nothing }


renderBlockItem : ConfigInternal msg -> StateInternal -> ShortBlock -> Html msg
renderBlockItem conf state block =
    Card.card { cardConfig | additionalAttributes = [ Elevation.z1 ] }
        { blocks =
            Card.cardPrimaryAction
                { cardPrimaryActionConfig | onClick = Just <| conf.toMsg <| toggleBlockOpen block.id state }
                [ Card.cardBlock <|
                    lazy2 renderBlockContent (state.blockState |> Dict.get block.id |> Maybe.withDefault False) block
                ]
        , actions =
            Just <|
                Card.cardActions
                    { buttons =
                        [ renderDetailsButton conf block.id
                        , block.entities |> List.map .id |> renderFindUsageButton conf
                        ]
                    , icons = []
                    }
        }


renderDetailsButton conf id =
    Card.cardActionButton { buttonConfig | onClick = Just <| conf.toDetailMsg id } "details"


renderFindUsageButton conf ids =
    Card.cardActionButton { buttonConfig | onClick = Just <| conf.toUsageMsg ids } "find usage"


renderDocContent : Documentation -> Html msg
renderDocContent doc =
    div []
        [ div [ Typography.caption, style "margin-bottom" "10pt" ] [ text doc.file ]
        , pre [ Typography.body1 ] [ renderHtml doc.src ]
        ]


renderBlockContent : Bool -> ShortBlock -> Html msg
renderBlockContent open block =
    Grid.layoutGrid [ Grid.alignLeft ]
        [ div [ Typography.caption, style "margin-bottom" "10pt" ] [ text block.file ]
        , pre [ Typography.body1 ] [ renderHtml block.src ]
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
    Badge.pillSecondary [ style "margin-right" "10px", Typography.body2 ] [ text label ]
