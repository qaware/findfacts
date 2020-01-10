module Entities exposing (Kind, ResultList, ShortResult, decoder, kindToString)

import Json.Decode exposing (Decoder, andThen, fail, field, int, list, map6, string, succeed)



-- Entities


type alias ResultList =
    List ShortResult


type alias ShortResult =
    { id : String
    , kind : Kind
    , sourceFile : String
    , startPosition : Int
    , endPosition : Int
    , shortDescription : String
    }


type Kind
    = Constant
    | Documentation
    | Fact
    | Type



-- STRING


kindToString : Kind -> String
kindToString kind =
    case kind of
        Constant ->
            "Constant"

        Documentation ->
            "Documentation"

        Fact ->
            "Fact"

        Type ->
            "Type"



-- JSON


decoder : Decoder ResultList
decoder =
    list shortDecoder


kindDecoder : Decoder Kind
kindDecoder =
    string |> andThen kindFromString


kindFromString : String -> Decoder Kind
kindFromString string =
    case string of
        "Constant" ->
            succeed Constant

        "Documentation" ->
            succeed Documentation

        "Fact" ->
            succeed Fact

        "Type" ->
            succeed Type

        _ ->
            fail ("Invalid kind: " ++ string)


shortDecoder : Decoder ShortResult
shortDecoder =
    map6 ShortResult
        (field "id" string)
        (field "kind" kindDecoder)
        (field "sourceFile" string)
        (field "startPosition" int)
        (field "endPosition" int)
        (field "shortDescription" string)
