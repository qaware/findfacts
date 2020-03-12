module Material.Extra.Typography exposing (code1, code2)

{-| This module defines additional fonts to use with material design.

@docs code1, code2

-}

import Html exposing (Attribute, Html)
import Html.Attributes exposing (class)


{-| Code font.
-}
code1 : Html.Attribute msg
code1 =
    class "mdc-typography-extra--code1"


{-| Light code font.
-}
code2 : Html.Attribute msg
code2 =
    class "mdc-typography-extra--code2"
