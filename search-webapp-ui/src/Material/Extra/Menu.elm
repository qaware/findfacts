module Material.Extra.Menu exposing
    ( Config, State
    , config, onClose, open, closed, subscriptions, view
    )

{-| Variant of material menu that closes on subscriptions rather than javascript state (which can cause side-effects).


# Types

@docs Config, State


# Component lifecycle

@docs config, onClose, open, closed, subscriptions, view

-}

import Browser.Events
import Html exposing (Attribute, Html, div)
import Json.Decode as Decode exposing (Decoder)
import Material.List exposing (listConfig)
import Material.Menu exposing (menuConfig)


{-| Opaque component configuration.
-}
type Config msg
    = Config (ConfigInternal msg)


type alias ConfigInternal msg =
    { toMsg : State -> msg
    , onClose : State -> msg
    }


{-| Creates a new configuration.
-}
config : (State -> msg) -> Config msg
config toMsg =
    Config <| ConfigInternal toMsg toMsg


{-| Sets a custom handler for when the menu would be closed.
-}
onClose : (State -> msg) -> Config msg -> Config msg
onClose toMsg (Config conf) =
    Config <| { conf | onClose = toMsg }


{-| Opaque component state.
-}
type State
    = Closed
    | Opening
    | Open


{-| Creates a new state with closed menu.
-}
closed : State
closed =
    Closed


{-| Creates a new state with open menu.
-}
open : State
open =
    Opening


{-| Subscriptions for this component.
-}
subscriptions : State -> Config msg -> Sub msg
subscriptions state (Config conf) =
    case state of
        Opening ->
            -- Delay activation one frame, to not register click twice
            Browser.Events.onAnimationFrame (always <| conf.toMsg Open)

        Open ->
            Browser.Events.onClick (outsideDecoder <| conf.onClose Closed)

        Closed ->
            Sub.none


outsideDecoder : msg -> Decoder msg
outsideDecoder msg =
    Decode.field "target" isInsideDecoder
        |> Decode.andThen
            (\inside ->
                if inside then
                    Decode.fail "inside"

                else
                    Decode.succeed msg
            )


isInsideDecoder : Decoder Bool
isInsideDecoder =
    Decode.oneOf
        [ Decode.at [ "className" ] Decode.string
            |> Decode.andThen
                (\s ->
                    if String.contains "mdc-menu" s then
                        Decode.succeed True

                    else
                        Decode.fail "parentElement"
                )
        , Decode.lazy (\_ -> Decode.field "parentElement" isInsideDecoder)
        , Decode.succeed False
        ]


{-| View for this component.
-}
view : State -> Html msg -> List (Material.List.ListItem msg) -> Html msg
view state anchor menuItems =
    div [ Material.Menu.menuSurfaceAnchor ]
        [ anchor
        , Material.Menu.menu
            { menuConfig | open = state == Open || state == Opening }
            [ Material.List.list { listConfig | wrapFocus = True } menuItems ]
        ]
