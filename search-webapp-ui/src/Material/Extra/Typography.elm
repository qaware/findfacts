module Material.Extra.Typography exposing (code, codeLight)

{-| This module defines additional fonts to use with material design.

@docs code, codeLight

-}

import Html exposing (Attribute, Html)
import Html.Attributes exposing (style)
import Util


{-| Code font.
-}
code : List (Html.Attribute msg) -> String -> Html msg
code additionalAttrs src =
    codeInternal additionalAttrs
        [ style "font-family" "Roboto Mono, monospace"
        , style "font-size" "16px"
        , style "font-weight" "400"
        , style "letter-spacing" "0.5px"
        ]
        src


{-| Light code font.
-}
codeLight : List (Html.Attribute msg) -> String -> Html msg
codeLight additionalAttrs src =
    codeInternal additionalAttrs
        [ style "font-family" "Roboto Mono, monospace"
        , style "font-size" "16px"
        , style "font-weight" "300"
        , style "letter-spacing" "0.5px"
        ]
        src


codeInternal : List (Html.Attribute msg) -> List (Html.Attribute msg) -> String -> Html msg
codeInternal codAttrs preAttrs src =
    Html.code codAttrs [ Html.pre preAttrs [ Html.map never <| Util.renderHtml src ] ]
