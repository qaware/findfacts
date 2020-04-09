module Util exposing
    ( ite, toMaybe
    , trailingBlanklines, stripTrailingBlankline, stripTrailingBlanklines
    , singletonIf, consIf, appIf
    , pairWith
    , anyDictDecoder, resultStringDecoder
    , renderHtml
    )

{-| Utility module.


# Common language helpers

@docs ite, toMaybe


# String utils

@docs trailingBlanklines, stripTrailingBlankline, stripTrailingBlanklines


# List conditional helpers

@docs singletonIf, consIf, appIf


# Tuple helpers

@docs pairWith


# Decoding helper

@docs dictDecoder, anyDictDecoder, resultStringDecoder


# HTML helper

@docs renderHtml

-}

import Dict.Any as AnyDict exposing (AnyDict)
import Html exposing (Html, div, text)
import Html.Attributes exposing (style)
import Html.Parser
import Html.Parser.Util
import Json.Decode as Decode exposing (Decoder, Error)
import Json.Decode.Extra as DecodeExtra
import List.Extra
import Result.Extra
import String.Extra


{-| Ternary operator. Unfortunately no infix notation since that's not allowed in user code any more.

    ite True 1 2 == 1

    ite False 1 2 == 2

-}
ite : Bool -> a -> a -> a
ite cond a b =
    if cond then
        a

    else
        b


{-| Converts a condition and an element to a Maybe.

    toMaybe 1 True == Just 1

    toMaybe 1 False == Nothing

-}
toMaybe : a -> Bool -> Maybe a
toMaybe a cond =
    if cond then
        Just a

    else
        Nothing


{-| Appends elem if the condition is met, or otherwise leaves it unchanged.

    appIf True 3 [ 1, 2 ] == [ 1, 2, 3 ]

    appIf False 3 [ 1, 2 ] == [ 1, 2 ]

-}
appIf : Bool -> a -> List a -> List a
appIf cond x xs =
    ite cond (xs ++ [ x ]) xs


{-| Constructs a list with the element in front if the condition is met, or otherwise leave the list unchanged.

    consIf True 1 [ 2, 3 ] == [ 1, 2, 3 ]

    consIf False 1 [ 2, 3 ] == [ 2, 3 ]

-}
consIf : Bool -> a -> List a -> List a
consIf cond x xs =
    ite cond (x :: xs) xs


{-| Constructs a singleton or empty list, depending on the condition.

    singletonIf True 1 == [ 1 ]

    singletonIf False 1 == []

-}
singletonIf : Bool -> a -> List a
singletonIf b a =
    consIf b a []


{-| Pair method to specify value first.

    "key" |> pairWith "value" == ( "key", "value" )

-}
pairWith : b -> a -> ( a, b )
pairWith b a =
    Tuple.pair a b


{-| Gives the trailing blank lines of a string.

    trailingBlanklines "foo\n bar\n\n " == [ "", " " ]

-}
trailingBlanklines : String -> List String
trailingBlanklines str =
    String.lines str |> List.Extra.takeWhileRight String.Extra.isBlank


{-| Strips the trailing blank line(s) from a string, if it has some.

    stripTrailingBlanklines "foo\n bar\n\n " == "foo\n bar"

-}
stripTrailingBlanklines : String -> String
stripTrailingBlanklines str =
    String.lines str |> List.Extra.dropWhileRight String.Extra.isBlank |> List.intersperse "\n" |> String.concat


{-| Strips a single trailing newline from a string, if there is one.

    stripTrailingBlankline "foo\n bar\n\n " == "foo\n bar\n"

-}
stripTrailingBlankline : String -> String
stripTrailingBlankline str =
    let
        lines =
            String.lines str
    in
    List.Extra.last lines
        |> Maybe.map
            (\l ->
                if String.Extra.isBlank l then
                    lines |> List.take (List.length lines - 1) |> List.intersperse "\n" |> String.concat

                else
                    str
            )
        |> Maybe.withDefault str


listKeyFold : (String -> Result String k) -> ( String, v ) -> Result String (List ( k, v )) -> Result String (List ( k, v ))
listKeyFold kDecoder ( kStr, v ) lRes =
    Result.andThen
        (\l ->
            kDecoder kStr
                |> Result.mapError (always <| "Error decoding key: " ++ kStr)
                |> Result.map (\k -> ( k, v ) :: l)
        )
        lRes


listKeyDecoder : (String -> Result String k) -> List ( String, v ) -> Decoder (List ( k, v ))
listKeyDecoder kDecoder l =
    List.foldl (listKeyFold kDecoder) (Ok []) l
        |> Result.map Decode.succeed
        |> Result.mapError Decode.fail
        |> Result.Extra.merge


{-| Decoder for AnyDicts. Keys in json are always strings, so for keys a string decoding function is needed.

    anyDictDecoder keyDecoder valueDecoder compareFn

-}
anyDictDecoder : (String -> Result String k) -> Decoder v -> (k -> comparable) -> Decoder (AnyDict comparable k v)
anyDictDecoder kDecoder vDecoder compare =
    Decode.keyValuePairs vDecoder
        |> Decode.andThen (listKeyDecoder kDecoder)
        |> Decode.map (AnyDict.fromList compare)


{-| Builds a decoder from a fromString method.

    type Day
        = Weekday
        | Weekend

    fromString: String -> Result String Day
    fromString str =
        case str of
            "Weekday" ->
                Ok Weekday

            "Weekend" ->
                Ok Weekend

            _ ->
                Err <| "No such day: " ++ str

    resultStringDecoder fromString

-}
resultStringDecoder : (String -> Result String a) -> Decoder a
resultStringDecoder fromString =
    Decode.string
        |> Decode.andThen
            (fromString >> DecodeExtra.fromResult)


{-| Parses string to HTML and appends to div.

    renderHtml "<br>Text with html"

-}
renderHtml : String -> Html Never
renderHtml html =
    case Html.Parser.run html of
        Ok nodes ->
            div [ style "display" "inline-block" ] <| Html.Parser.Util.toVirtualDom nodes

        _ ->
            text (String.Extra.stripTags html)
