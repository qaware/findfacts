module Material.Extra.Divider exposing (divider, dividerWith)

{-| Material module for dividers.

@docs divider, dividerWith

-}

import Html exposing (Attribute, Html, div)
import Html.Attributes exposing (class)


{-| MDC divider.
-}
divider : Html msg
divider =
    div [ class "mdc-list-divider" ] []


{-| MDC divider with additional attributes.
-}
dividerWith : List (Attribute msg) -> Html msg
dividerWith attrs =
    div (class "mdc-list-divider" :: attrs) []
