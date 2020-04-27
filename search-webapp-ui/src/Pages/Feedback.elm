module Pages.Feedback exposing (Config, config, view)

{-| Module for the (mostly static) feedback page.
-}

import Html exposing (Html, a, div, h1, p, text)
import Html.Attributes exposing (href)
import Material.Extra.Obfuscated as Obfuscated
import Material.Typography as Typography


{-| Opaque config type.
-}
type Config msg
    = Config { toMsg : Obfuscated.State -> msg }


{-| Creates a new config.
-}
config : (Obfuscated.State -> msg) -> Config msg
config toMsg =
    Config { toMsg = toMsg }


{-| Renders the 'feedback' page.
-}
view : Obfuscated.State -> Config msg -> Html msg
view obfuscated (Config { toMsg }) =
    div []
        [ h1 [ Typography.headline3 ] [ text "Feedback" ]
        , p [ Typography.body1 ]
            [ text "All feedback is greatly appreciated! Also feel free to ask any questions." ]
        , p [ Typography.body1 ]
            [ text "Simply write a message to "
            , Obfuscated.config toMsg
                |> Obfuscated.withDisplay
                    (\s ->
                        a [ href <| "mailto:" ++ s ++ "?subject=[FindFacts] Feedback..." ]
                            [ text s ]
                    )
                |> Obfuscated.withObfuscate (\e -> a [ href "" ] [ e ])
                |> Obfuscated.view obfuscated
            , text ". If you have a specific question, please include the URL!"
            ]
        , p [ Typography.body1 ]
            [ text "Please also consider filling out this short "
            , a [ href "https://forms.gle/K7Dmae9m5uVViPb57" ] [ text "survey" ]
            , text ". It won't take more than five minutes."
            ]
        ]
