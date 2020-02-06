module Entities exposing (Kind(..), ResultList, ShortBlock, ShortEt, compareByKind, decoder, kindToString)

import Json.Decode as Decode exposing (Decoder, list)



-- Entities


type alias ResultList =
    List ShortBlock


type alias ShortBlock =
    { id : String
    , file : String
    , src : String
    , entities : List ShortEt
    }


type alias ShortEt =
    { id : String
    , kind : Kind
    , name : String
    , shortDescription : String
    }


type Kind
    = Constant
    | Fact
    | Type



-- UTIL


compareByKind : ShortEt -> Int
compareByKind et =
    case et.kind of
        Type ->
            0

        Constant ->
            1

        Fact ->
            2



-- STRING


kindToString : Kind -> String
kindToString kind =
    case kind of
        Constant ->
            "Constant"

        Fact ->
            "Fact"

        Type ->
            "Type"


kindFromString : String -> Decoder Kind
kindFromString string =
    case string of
        "Constant" ->
            Decode.succeed Constant

        "Fact" ->
            Decode.succeed Fact

        "Type" ->
            Decode.succeed Type

        _ ->
            Decode.fail ("Invalid theory entity kind: " ++ string)



-- JSON


decoder : Decoder ResultList
decoder =
    Decode.list blockDecoder


kindDecoder : Decoder Kind
kindDecoder =
    Decode.string |> Decode.andThen kindFromString


blockDecoder : Decoder ShortBlock
blockDecoder =
    Decode.map4 ShortBlock
        (Decode.field "id" Decode.string)
        (Decode.field "file" Decode.string)
        (Decode.field "src" Decode.string)
        (Decode.field "entities" childDecoder)


childDecoder : Decoder (List ShortEt)
childDecoder =
    list shortDecoder


shortDecoder : Decoder ShortEt
shortDecoder =
    Decode.map4 ShortEt
        (Decode.field "id" Decode.string)
        (Decode.field "kind" kindDecoder)
        (Decode.field "name" Decode.string)
        (Decode.field "description" Decode.string)
