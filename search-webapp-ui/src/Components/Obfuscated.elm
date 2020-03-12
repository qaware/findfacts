module Components.Obfuscated exposing
    ( Config, State
    , config, withDisplay, withObfuscate
    , init, view
    )

{-| Component to hide sensitive information, such as email addresses, from being scraped.


# Types

@docs Config, State


# Configuration

@docs config, withDisplay, withObfuscate


# Lifecycle

@docs init, view

-}

import Html exposing (Html)
import Html.Attributes as Attributes
import Html.Events as Events


{-| Opaque configuration type for this pure component.
-}
type Config msg
    = Config (ConfigInternal msg)


type alias ConfigInternal msg =
    { toMsg : State -> msg
    , toDisplay : String -> Html msg
    , toObfuscated : Html msg -> Html msg
    }


{-| Opaque state type for this pure component.
-}
type State
    = State StateInternal


type alias StateInternal =
    { open : Bool
    , content : List String
    }


{-| Create a initial configuration, with the message type for this pure component.
-}
config : (State -> msg) -> Config msg
config toMsg =
    Config <| ConfigInternal toMsg Html.text identity


{-| Configure how the clear text should be displayed, once the obfuscated element had a mouse over event.
-}
withDisplay : (String -> Html msg) -> Config msg -> Config msg
withDisplay fn (Config conf) =
    Config { conf | toDisplay = fn }


{-| Configure how the obfuscated element should be wrapped. Note that the wrapped element must be able to get an
onMouseOver event!
-}
withObfuscate : (Html msg -> Html msg) -> Config msg -> Config msg
withObfuscate fn (Config conf) =
    Config { conf | toObfuscated = fn }


{-| Creates a new state from the string that should be obfuscated.
To make sure that no one reads the input string from this method call, encrypt the string literal in the code and only
decrypt it at runtime.
-}
init : String -> State
init content =
    let
        total =
            ceiling <| toFloat (String.length content) / 3.0
    in
    State <|
        StateInternal False
            (List.range 0 (total - 1) |> List.map (\i -> String.slice (3 * i) (3 * i + 3) content |> String.reverse))


{-| Renders the obfuscated element.
-}
view : State -> Config msg -> Html msg
view (State state) (Config conf) =
    if state.open then
        conf.toDisplay (state.content |> List.map String.reverse |> String.concat)

    else
        Html.span [ Events.onMouseOver <| conf.toMsg <| State { state | open = True } ]
            [ obfuscate state.content |> conf.toObfuscated ]



-- INTERNALS


obfuscate : List String -> Html msg
obfuscate chars =
    chars
        |> List.reverse
        |> List.map
            (\t ->
                Html.span [ Attributes.style "unicode-bidi" "bidi-override", Attributes.style "direction" "rtl" ]
                    [ obfuscator, Html.text t, obfuscator ]
            )
        |> Html.span []


obfuscator : Html msg
obfuscator =
    Html.span [ Attributes.style "display" "none" ] [ Html.text "a" ]
