module Pages.About exposing (view)

{-| The static 'about' page.
-}

import Html exposing (Html, a, h1, h2, p, text)
import Html.Attributes exposing (href, rel, target)
import Material.Typography as Typography


{-| Renders the 'about' page.
-}
view : String -> List (Html msg)
view version =
    [ h1 [ Typography.headline3 ] [ text "About" ]
    , h2 [ Typography.overline ] [ text <| "Version: " ++ version ]
    , p [ Typography.body1 ]
        [ text "This is a search application to find formal theory content of "
        , a [ href "https://isabelle.in.tum.de/" ] [ text "Isabelle" ]
        , text " and the "
        , a [ href "https://www.isa-afp.org/" ] [ text "AFP" ]
        , text "."
        ]
    , p [ Typography.body1 ]
        [ text "The development is part of my master thesis at the "
        , a [ href "http://www21.in.tum.de/index" ] [ text "Chair for Logic and Verification" ]
        , text " at TUM, in cooperation with "
        , a [ href "https://www.qaware.de/" ] [ text "QAware Software Engineering" ]
        , text "."
        ]
    , p [ Typography.body1 ]
        [ text "Source code can be found in the "
        , a [ href "https://github.com/qaware/findfacts" ] [ text "github repository" ]
        , text "."
        ]
    , p [ Typography.body1 ]
        [ text "Documentation of the query REST API can be found under "
        , a [ href "/docs", rel "external", target "_self" ] [ text "/docs" ]
        , text "."
        ]
    , p [ Typography.body1 ]
        [ text "If you encounter any bugs, unexpected behaviour, unclear UI, or have any suggestions, please leave some "
        , a [ href "#feedback" ] [ text "feedback" ]
        , text "."
        ]
    ]
