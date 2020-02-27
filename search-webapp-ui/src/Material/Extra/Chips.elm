module Material.Extra.Chips exposing
    ( Config, InputChip, State
    , config, input, inputSet, view
    )

{-| Module for input chips, since the material design components has issues with its underlying js.


# Types

@docs Config, InputChip, State


# Component

@docs config, input, inputSet, view

-}

import Html exposing (Html, div, i, node, text)
import Html.Attributes exposing (attribute, class, style)
import Html.Events as Events
import List.Extra as ListExtra


type Config msg
    = Config (List String -> msg)


config : (List String -> msg) -> Config msg
config toMsg =
    Config toMsg


type InputChip
    = InputChip String


input : String -> InputChip
input chip =
    InputChip chip


type State
    = State (List String)


inputSet : List InputChip -> State
inputSet chips =
    State (chips |> List.map (\(InputChip c) -> c))


view : Config msg -> State -> Html msg
view (Config toMsg) (State chips) =
    renderInputChipSet toMsg chips


renderInputChipSet : (List String -> msg) -> List String -> Html msg
renderInputChipSet toMsg chips =
    div [ class "mdc-chip-set", class "mdc-chip-set--input" ]
        (chips |> List.indexedMap (\idx -> renderInputChip (\() -> toMsg <| ListExtra.removeAt idx chips)))


renderInputChip : (() -> msg) -> String -> Html msg
renderInputChip toMsg chip =
    node "mdc-chip"
        [ class "mdc-chip" ]
        [ div [ class "mdc-chip__text" ] [ text chip ]
        , i
            [ class "material-icons mdc-chip__icon"

            -- Style to replace the mdc-chip__icon--trailing class that causes issues
            , style "width" "18px"
            , style "height" "18px"
            , style "font-size" "18px"
            , style "margin-right" "-4px"
            , style "margin-left" "4px"
            , style "color" "rgba(0,0,0,.54)"
            , attribute "tabindex" "0"
            , attribute "role" "button"
            , Events.onClick <| toMsg ()
            ]
            [ text "close" ]
        ]
