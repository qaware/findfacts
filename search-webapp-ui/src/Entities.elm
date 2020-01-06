module Entities exposing (Kind, kindDecoder, ShortResult, shortDecoder)

import Json.Decode exposing (Decoder, map6, field, string, int, andThen, succeed, fail)

type Kind
  = Constant
  | Documentation
  | Fact
  | Type

type alias ShortResult =
  { id: String
  , kind: Kind
  , source: String
  , startPosition: Int
  , endPosition: Int
  , shortDescription: String
   }


-- JSON

kindDecoder : Decoder Kind
kindDecoder = string |> andThen kindFromString

kindFromString: String -> Decoder Kind
kindFromString string =
    case string of
        "Constant" -> succeed Constant
        "Documentation" -> succeed Documentation
        "Fact" -> succeed Fact
        "Type" -> succeed Type
        _ -> fail ("Invalid kind: " ++ string)

shortDecoder : Decoder ShortResult
shortDecoder =
   map6 ShortResult
       (field "id" string)
       (field "kind" kindDecoder)
       (field "source" string)
       (field "startPosition" int)
       (field "endPosition" int)
       (field "shortDescription" string)