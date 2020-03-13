module Material.Extra.Code exposing
    ( Config, block, withMaxHeight, withContext, withLineNumbersFrom, withAdditionalAttrs
    , view
    )

{-| Module for code components.


# Builders

@docs Config, block, withStart, withMaxHeight, withContext, withLineNumbersFrom, withAdditionalAttrs


# Component

@docs view

-}

import Html exposing (Html, code, div, pre)
import Html.Attributes exposing (style)
import Html.Lazy exposing (lazy3)
import Material.Extra.Divider as Divider
import Material.Extra.Typography as ExtraTypography
import Material.Theme as Typography
import Util exposing (renderHtml)


{-| Opaque config type.
-}
type Config msg
    = Config (ConfigInternal msg)


type alias ConfigInternal msg =
    { src : String
    , startLine : Maybe Int
    , maxHeight : Maybe Int
    , context : Maybe ( String, String )
    , additionalAttrs : List (Html.Attribute msg)
    }


{-| New code block from string.
-}
block : String -> Config msg
block src =
    Config <| ConfigInternal src Nothing Nothing Nothing []


{-| Adds line numbers from the given start line.
-}
withLineNumbersFrom : Int -> Config msg -> Config msg
withLineNumbersFrom start (Config conf) =
    Config { conf | startLine = Just start }


{-| Adds a max-height (after which the user needs to scroll) to the code block.
-}
withMaxHeight : Int -> Config msg -> Config msg
withMaxHeight height (Config conf) =
    Config { conf | maxHeight = Just height }


{-| Adds context before and after the code block.
-}
withContext : String -> String -> Config msg -> Config msg
withContext before after (Config conf) =
    Config { conf | context = Just ( before, after ) }


{-| Adds additional attributes to the code block.
-}
withAdditionalAttrs : List (Html.Attribute msg) -> Config msg -> Config msg
withAdditionalAttrs attrs (Config conf) =
    Config { conf | additionalAttrs = attrs }


{-| Renders the code block.
-}
view : Config msg -> Html msg
view (Config conf) =
    let
        innerBlock =
            lazy3 renderBlock conf.src ExtraTypography.code1 conf.startLine

        htmlBlocks =
            case conf.context of
                Just ( before, after ) ->
                    List.concat
                        [ if String.isEmpty before then
                            []

                          else
                            [ lazy3 renderBlock
                                before
                                ExtraTypography.code2
                                (conf.startLine |> Maybe.map (\l -> l - (List.length <| String.lines before)))
                            , Divider.divider
                            ]
                        , [ innerBlock ]
                        , if String.isEmpty after then
                            []

                          else
                            [ Divider.divider
                            , lazy3 renderBlock
                                after
                                ExtraTypography.code2
                                (conf.startLine |> Maybe.map (\l -> l + (List.length <| String.lines conf.src)))
                            ]
                        ]

                Nothing ->
                    [ innerBlock ]
    in
    Html.div (conf.additionalAttrs ++ (conf.maxHeight |> Maybe.map maxDiv |> Maybe.withDefault [])) htmlBlocks



-- RENDERING


renderBlock : String -> Html.Attribute msg -> Maybe Int -> Html msg
renderBlock src font startLine =
    startLine |> Maybe.map (renderLineNumbers src font) |> Maybe.withDefault (renderCode src font)


renderLineNumbers : String -> Html.Attribute msg -> Int -> Html msg
renderLineNumbers src font start =
    let
        numLines =
            List.length <| String.lines src

        linesBlock =
            List.range start (start + numLines - 1)
                |> List.map String.fromInt
                |> List.intersperse "\n"
                |> (String.concat >> Html.text >> List.singleton)
                |> Html.pre
                    [ Typography.textHintOnLight
                    , style "text-align" "right"
                    , style "min-width" "5ch"
                    , style "float" "left"
                    , style "margin-right" "16px"
                    ]
    in
    div [ style "display" "inline-flex" ] [ linesBlock, renderCode src font ]


renderCode : String -> Html.Attribute msg -> Html msg
renderCode src font =
    code [ style "float" "left" ] [ pre [ font ] [ Html.map never <| renderHtml src ] ]


maxDiv : Int -> List (Html.Attribute msg)
maxDiv height =
    [ style "max-height" <| String.fromInt height ++ "px", style "overflow-y" "auto" ]
