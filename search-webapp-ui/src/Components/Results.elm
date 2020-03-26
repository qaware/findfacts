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
import Html exposing (Html, br, div, span, text)
import Html.Attributes exposing (style)
import Html.Lazy exposing (lazy, lazy2)
import Material.Button exposing (buttonConfig)
import Material.Card as Card exposing (cardConfig, cardPrimaryActionConfig)
import Material.Elevation as Elevation
import Material.Extra.Code as Code
import Material.LayoutGrid as Grid
import Material.LinearProgress as Progress
import Material.Theme as Theme
import Material.Typography as Typography


{-| Opaque config type for the results component.
-}
type Config msg
    = Config (ConfigInternal msg)


type alias ConfigInternal msg =
    { toMsg : State -> msg
    , toDetailMsg : String -> msg
    , toUsedByMsg : String -> List String -> msg
    , toUsesMsg : String -> List String -> msg
    }


{-| Opaque state type for the results component.
-}
type State
    = Empty
    | Searching
    | Error String
    | Values StateInternal


type alias StateInternal =
    { blocks : List ShortBlock }


{-| Creates a config for a results component.
-}
config : (State -> msg) -> (String -> msg) -> (String -> List String -> msg) -> (String -> List String -> msg) -> Config msg
config toMsg toDetailMsg toUsedByMsg toUsesMsg =
    Config (ConfigInternal toMsg toDetailMsg toUsedByMsg toUsesMsg)


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
                { blocks = resultList.values }

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
                        |> List.map (lazy2 renderBlock conf)
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
-- RENDERING


renderBlock : ConfigInternal msg -> ShortBlock -> Html msg
renderBlock conf block =
    let
        content =
            lazy renderBlockContent block

        ids =
            block.entities |> List.map .id
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
                            [ renderFindUsedByButton conf block.src ids, renderFindUsesButton conf block.src ids ]
                        , icons = []
                        }
        }


renderFindUsedByButton conf block ids =
    Card.cardActionButton { buttonConfig | onClick = Just <| conf.toUsedByMsg block ids } "used by"


renderFindUsesButton conf block ids =
    Card.cardActionButton { buttonConfig | onClick = Just <| conf.toUsesMsg block ids } "uses"


renderBlockContent : ShortBlock -> Html msg
renderBlockContent block =
    Grid.layoutGrid [ Grid.alignLeft, style "width" "100%" ]
        [ div [ Typography.caption, style "margin-bottom" "10pt" ] [ text block.file ]
        , Code.block (String.trimRight block.src)
            |> Code.withLineNumbersFrom block.startLine
            |> lazy Code.view
        , lazy renderEntitySummary block.entities
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
