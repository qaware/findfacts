module MaterialExtra exposing (divider)

{-| Module for custom material components.

@docs divider

-}

import Html exposing (Html, div)
import Html.Attributes exposing (class)


divider : Html msg
divider =
    div [ class "mdc-list-divider" ] []
