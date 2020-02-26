module Material.Extra.Typography exposing (code)

{-| This module defines additional fonts to use with material design.

@docs code

-}

import Html exposing (Attribute, Html)
import Html.Attributes exposing (style)
import Util


{-| Code font.
-}
code : List (Html.Attribute msg) -> String -> Html msg
code additionalAttributes src =
    Html.code
        [ style "font-family" "monospace"
        , style "font-size" "16px"
        , style "font-weight" "400"
        , style "letter-spacing" "0.5px"
        ]
        [ Html.pre [] [ Html.map never <| Util.renderHtml src ]
        ]
