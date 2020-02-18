module Util exposing
    ( ite, toMaybe
    , singletonIf, consIf, appIf
    , pairWith
    , anyDictDecoder
    , renderHtml
    )

{-| Utility module.


# Common language helpers

@docs ite, toMaybe


# List conditional helpers

@docs singletonIf, consIf, appIf


# Tuple helpers

@docs pairWith


# Decoding helper

@docs anyDictDecoder


# HTML helper

@docs renderHtml

-}

import Dict.Any as AnyDict exposing (AnyDict)
import Html exposing (Html, div, text)
import Html.Parser
import Html.Parser.Util
import Json.Decode as Decode exposing (Decoder)


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


{-| Decoder for AnyDicts.

    anyDictDecoder keyDecoder valueDecoder compareFn

-}
anyDictDecoder : (String -> Decoder k) -> Decoder v -> (k -> comparable) -> Decoder (AnyDict comparable k v)
anyDictDecoder kFromString vDecoder compare =
    Decode.keyValuePairs vDecoder
        -- Decode (List (String, v))
        |> Decode.andThen (\l -> List.foldl (\( kStr, v ) dec -> Decode.map2 (\k xs -> ( k, v ) :: xs) (kFromString kStr) dec) (Decode.succeed []) l)
        -- Decoder (List (k, v))
        |> Decode.map (AnyDict.fromList compare)


{-| Parses string to HTML and appends to div.

    renderHtml "<br>Text with html"

-}
renderHtml : String -> Html msg
renderHtml html =
    case Html.Parser.run html of
        Ok nodes ->
            div [] <| Html.Parser.Util.toVirtualDom nodes

        _ ->
            text html
