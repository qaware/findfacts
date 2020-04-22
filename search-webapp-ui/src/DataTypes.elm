module DataTypes exposing (..)

{-| This module contains business logic data types and their string/json encodings and decoders.
They are not individually documented here, see backend documentation (e.g. in /docs) for details.
-}

-- ENTITY TYPES

import Dict exposing (Dict)
import Dict.Any exposing (AnyDict)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value, object)
import Maybe.Extra
import Util exposing (anyDictDecoder, resultStringDecoder)


type alias ResultList a =
    { count : Int
    , nextCursor : String
    , values : List a
    }


type alias ShortBlock =
    { id : String
    , file : String
    , command : String
    , startLine : Int
    , srcBefore : String
    , src : String
    , srcAfter : String
    , entities : List ShortEt
    }


type alias ShortEt =
    { id : String
    , kind : Kind
    , name : String
    }


type Kind
    = Constant
    | Fact
    | Type


type ThyEt
    = ThyConstant ConstantEt
    | ThyFact FactEt
    | ThyType TypeEt


type alias ConstantEt =
    { id : String
    , typ : String
    , uses : List ShortEt
    }


type alias FactEt =
    { id : String
    , uses : List ShortEt
    }


type alias TypeEt =
    { id : String
    , uses : List ShortEt
    }



-- QUERY TYPES


type Field
    = Id
    | ChildId
    | SrcFile
    | SrcFileFacet
    | Command
    | StartLine
    | SrcBefore
    | Src
    | SrcAfter
    | Name
    | NameFacet
    | Kind
    | ConstType
    | ConstTypeFacet
    | Uses


type Filter
    = Term String
    | Exact String
    | InRange Int Int
    | InResult Field (List FieldFilter)
    | Not Filter
    | And Filter Filter (List Filter)
    | Or Filter Filter (List Filter)


type alias FieldFilter =
    { field : Field
    , filter : Filter
    }


type alias FilterQuery =
    { filters : List FieldFilter
    , pageSize : Int
    , cursor : Maybe String
    }


type alias FacetQuery =
    { filters : List FieldFilter
    , fields : List Field
    , maxFacets : Int
    }


type alias ResultFacet =
    Dict String Int


type alias ResultFaceting =
    AnyDict String Field ResultFacet



-- STRING ENCODINGS


kindToString : Kind -> String
kindToString kind =
    case kind of
        Constant ->
            "Constant"

        Fact ->
            "Fact"

        Type ->
            "Type"


fieldToString : Field -> String
fieldToString field =
    case field of
        Id ->
            "Id"

        ChildId ->
            "ChildId"

        SrcFile ->
            "SourceTheory"

        SrcFileFacet ->
            "SourceTheoryFacet"

        Command ->
            "Command"

        StartLine ->
            "StartLine"

        SrcBefore ->
            "SourceTextBefore"

        Src ->
            "SourceText"

        SrcAfter ->
            "SourceTextAfter"

        Kind ->
            "Kind"

        Name ->
            "Name"

        NameFacet ->
            "NameFacet"

        ConstType ->
            "ConstantType"

        ConstTypeFacet ->
            "ConstantTypeFacet"

        Uses ->
            "Uses"



-- STRING DECODINGS


fieldFromString : String -> Result String Field
fieldFromString str =
    case str of
        "Id" ->
            Ok Id

        "ChildId" ->
            Ok ChildId

        "SourceTheory" ->
            Ok SrcFile

        "SourceTheoryFacet" ->
            Ok SrcFileFacet

        "Command" ->
            Ok Command

        "StartLine" ->
            Ok StartLine

        "SourceTextBefore" ->
            Ok SrcBefore

        "SourceText" ->
            Ok Src

        "SourceTextAfter" ->
            Ok SrcAfter

        "Kind" ->
            Ok Kind

        "Name" ->
            Ok Name

        "NameFacet" ->
            Ok NameFacet

        "ConstantType" ->
            Ok ConstType

        "ConstantTypeFacet" ->
            Ok ConstTypeFacet

        "Uses" ->
            Ok Uses

        _ ->
            Err ("No such field: " ++ str)


kindFromString : String -> Result String Kind
kindFromString str =
    case str of
        "Constant" ->
            Ok Constant

        "Fact" ->
            Ok Fact

        "Type" ->
            Ok Type

        _ ->
            Err <| "Invalid theory entity kind: " ++ str



-- JSON ENCODINGS


encodeField : Field -> Value
encodeField field =
    field |> fieldToString |> Encode.string


encodeFilter : Filter -> Value
encodeFilter filter =
    case filter of
        Term str ->
            object [ ( "Term", object [ ( "inner", Encode.string str ) ] ) ]

        Exact str ->
            object [ ( "Exact", object [ ( "inner", Encode.string str ) ] ) ]

        InRange from to ->
            object
                [ ( "InRange"
                  , object
                        [ ( "from", Encode.int from )
                        , ( "to", Encode.int to )
                        ]
                  )
                ]

        InResult field fs ->
            object
                [ ( "InResult"
                  , object
                        [ ( "ofField", encodeField field )
                        , ( "query", Encode.list encodeFieldFilter fs )
                        ]
                  )
                ]

        Not f ->
            object [ ( "Not", object [ ( "filter", encodeFilter f ) ] ) ]

        And f1 f2 fn ->
            object [ ( "And", object [ ( "f1", encodeFilter f1 ), ( "f2", encodeFilter f2 ), ( "fn", Encode.list encodeFilter fn ) ] ) ]

        Or f1 f2 fn ->
            object [ ( "Or", object [ ( "f1", encodeFilter f1 ), ( "f2", encodeFilter f2 ), ( "fn", Encode.list encodeFilter fn ) ] ) ]


encodeFieldFilter : FieldFilter -> Value
encodeFieldFilter filter =
    object [ ( "field", encodeField filter.field ), ( "filter", encodeFilter filter.filter ) ]


encodeFilterQuery : FilterQuery -> Value
encodeFilterQuery filterQuery =
    [ Just ( "filters", Encode.list encodeFieldFilter filterQuery.filters )
    , Just ( "pageSize", Encode.int filterQuery.pageSize )
    , filterQuery.cursor |> Maybe.map (\c -> ( "cursor", Encode.string c ))
    ]
        |> Maybe.Extra.values
        |> object


encodeFacetQuery : FacetQuery -> Value
encodeFacetQuery facetQuery =
    object
        [ ( "filters", Encode.list encodeFieldFilter facetQuery.filters )
        , ( "fields", Encode.list (\f -> f |> fieldToString |> Encode.string) facetQuery.fields )
        , ( "maxFacets", Encode.int facetQuery.maxFacets )
        ]



-- JSON DECODERS


resultListDecoder : Decoder a -> Decoder (ResultList a)
resultListDecoder aDecoder =
    Decode.map3 ResultList
        (Decode.field "count" Decode.int)
        (Decode.field "nextCursor" Decode.string)
        (Decode.field "values" (Decode.list aDecoder))


kindDecoder : Decoder Kind
kindDecoder =
    resultStringDecoder kindFromString


shortBlockDecoder : Decoder ShortBlock
shortBlockDecoder =
    Decode.map8 ShortBlock
        (Decode.field "id" Decode.string)
        (Decode.field "theory" Decode.string)
        (Decode.field "command" Decode.string)
        (Decode.field "startLine" Decode.int)
        (Decode.field "srcBefore" Decode.string)
        (Decode.field "src" Decode.string)
        (Decode.field "srcAfter" Decode.string)
        (Decode.field "entities" (Decode.list shortEtDecoder))


shortEtDecoder : Decoder ShortEt
shortEtDecoder =
    Decode.map3 ShortEt
        (Decode.field "id" Decode.string)
        (Decode.field "kind" kindDecoder)
        (Decode.field "name" Decode.string)


resultFacetDecoder : Decoder ResultFacet
resultFacetDecoder =
    Decode.dict Decode.int


fieldDecoder : Decoder Field
fieldDecoder =
    resultStringDecoder fieldFromString


resultFacetingDecoder : Decoder ResultFaceting
resultFacetingDecoder =
    anyDictDecoder fieldFromString resultFacetDecoder fieldToString


constantEtDecoder : Decoder ConstantEt
constantEtDecoder =
    Decode.map3 ConstantEt
        (Decode.field "id" Decode.string)
        (Decode.field "typ" Decode.string)
        (Decode.field "uses" (Decode.list shortEtDecoder))


factEtDecoder : Decoder FactEt
factEtDecoder =
    Decode.map2 FactEt
        (Decode.field "id" Decode.string)
        (Decode.field "uses" (Decode.list shortEtDecoder))


typeEtDecoder : Decoder TypeEt
typeEtDecoder =
    Decode.map2 TypeEt
        (Decode.field "id" Decode.string)
        (Decode.field "uses" (Decode.list shortEtDecoder))


thyEtDecoderFromKind : Kind -> Decoder ThyEt
thyEtDecoderFromKind kind =
    case kind of
        Constant ->
            Decode.map ThyConstant constantEtDecoder

        Fact ->
            Decode.map ThyFact factEtDecoder

        Type ->
            Decode.map ThyType typeEtDecoder


thyEtDecoder : Decoder ThyEt
thyEtDecoder =
    Decode.field "kind" kindDecoder |> Decode.andThen thyEtDecoderFromKind



-- OTHER


kindCompare : Kind -> Int
kindCompare kind =
    case kind of
        Constant ->
            0

        Type ->
            1

        Fact ->
            2
