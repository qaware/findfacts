module Pages.Help exposing (view)

{-| The static 'help' page.
-}

import Html exposing (Html, a, b, br, h1, h2, h3, p, text)
import Html.Attributes exposing (href)
import Material.Typography as Typography


{-| Renders the 'syntax' page.
-}
view : List (Html msg)
view =
    [ h1 [ Typography.headline3 ] [ text "Help" ]
    , p [ Typography.body1 ]
        [ text "Generally, "
        , b [] [ text "Inputs" ]
        , text " are split into "
        , b [] [ text "terms" ]
        , text " by special characters, such as whitespace, '.', '_', or '-'."
        ]
    , h2 [ Typography.headline4 ] [ text "Isabelle Characters" ]
    , p [ Typography.body1 ]
        [ text "To search for isabelle characters, use the abbreviation (if unique): "
        , a [ href "#search?q={\"term\"%3A\"%3D%3D>\"}" ] [ text "==>" ]
        , text ", the isabelle markup : "
        , a [ href "#search?q={\"term\"%3A\"\\\\<Longrightarrow>\"}" ] [ text "\\<Longrightarrow>" ]
        , text ", or the unicode representation: "
        , a [ href "#search?q={\"term\"%3A\"⟹\"}" ] [ text "⟹" ]
        , text "."
        ]
    , h2 [ Typography.headline4 ] [ text "Main Search Bar" ]
    , p [ Typography.body1 ]
        [ text "The main search bar will match for any of your search terms - listings results first where multiple terms match."
        , br [] []
        , text "'*' Wildcards are allowed so you don't need to be too specific."
        ]
    , h3 [ Typography.headline6 ] [ text "Example" ]
    , a [ Typography.typography, href "#search?q={\"term\"%3A\"inv*\"}" ]
        [ text "Searching for inverse, which might be abbreviated by 'inv'" ]
    , h2 [ Typography.headline4 ] [ text "Filters" ]
    , p [ Typography.body1 ]
        [ text "You can add filters that restrict your results further."
        , br [] []
        , text "Filters always target a specific field, and they will restrict results to match either of your inputs. However, for an input to match, all its terms must match."
        ]
    , h3 [ Typography.headline6 ] [ text "Example" ]
    , a [ Typography.typography, href "#search?&q={\"fields\"%3A[{\"field\"%3A\"Name\"%2C\"terms\"%3A[\"equal nat\"%2C\"equal int\"]}]}" ]
        [ text "Filtering for semantic entities with that are (from their name) about equality in ints or nats." ]
    , h2 [ Typography.headline4 ] [ text "Facets" ]
    , p [ Typography.body1 ]
        [ text "If you have restricted your search enough so there are only a handful of alternatives for a property, you can choose between the remaining options."
        , br [] []
        , text "Selecting multiple values will give you results that match either."
        ]
    , h3 [ Typography.headline6 ] [ text "Example" ]
    , a [ Typography.typography, href "#search?q={\"term\"%3A\"*\"%2C\"facets\"%3A{\"Kind\"%3A[\"Constant\"]}}" ]
        [ text "Restricting search to constants" ]
    ]
