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
import Util exposing (anyDictDecoder)


type alias ResultList a =
    { count : Int
    , nextCursor : String
    , values : List a
    }


type ShortCmd
    = Block ShortBlock
    | Doc Documentation


type alias ShortBlock =
    { id : String
    , file : String
    , src : String
    , entities : List ShortEt
    }


type alias Documentation =
    { id : String
    , file : String
    , src : String
    , docKind : DocumentationKind
    }


type DocumentationKind
    = Latex
    | Meta
    | Inline


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
    , typUses : List ShortEt
    , propUses : List ShortEt
    }


type alias FactEt =
    { id : String
    , propUses : List ShortEt
    , proofUses : List ShortEt
    }


type alias TypeEt =
    { id : String
    , ctorUses : List ShortEt
    }



-- QUERY TYPES


type FilterTerm
    = Term String
    | Exact String
    | InRange Int Int
    | AnyInResult AbstractFQ
    | AllInResult AbstractFQ


type AbstractFQ
    = Filter (List ( Field, FilterTerm ))
    | Intersection AbstractFQ AbstractFQ (List AbstractFQ)
    | Union AbstractFQ AbstractFQ (List AbstractFQ)
    | Complement AbstractFQ


type Field
    = Id
    | CmdKind
    | Src
    | SrcFile
    | Name
    | Kind
    | Prop
    | ConstType
    | ConstTypeFacet
    | DocKind


type alias FilterQuery =
    { filter : AbstractFQ
    , pageSize : Int
    , cursor : Maybe String
    }


type alias FacetQuery =
    { filter : AbstractFQ
    , fields : List Field
    , maxFacets : Int
    }


type alias Facet =
    Dict String Int


type alias FacetResult =
    AnyDict String Field Facet



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


documentationKindToString : DocumentationKind -> String
documentationKindToString kind =
    case kind of
        Latex ->
            "Latex"

        Meta ->
            "Meta"

        Inline ->
            "Inline"


fieldToString : Field -> String
fieldToString field =
    case field of
        Id ->
            "Id"

        CmdKind ->
            "CommandKind"

        Src ->
            "SourceText"

        SrcFile ->
            "SourceTheory"

        Kind ->
            "Kind"

        Name ->
            "Name"

        Prop ->
            "Proposition"

        ConstType ->
            "ConstantType"

        ConstTypeFacet ->
            "ConstantTypeFacet"

        DocKind ->
            "DocumentationKind"



-- STRING DECODERS


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


fieldFromString : String -> Decoder Field
fieldFromString str =
    case str of
        "Id" ->
            Decode.succeed Id

        "CommandKind" ->
            Decode.succeed CmdKind

        "SourceText" ->
            Decode.succeed Src

        "SourceTheory" ->
            Decode.succeed SrcFile

        "Kind" ->
            Decode.succeed Kind

        "Name" ->
            Decode.succeed Name

        "Proposition" ->
            Decode.succeed Prop

        "ConstantType" ->
            Decode.succeed ConstType

        "ConstantTypeFacet" ->
            Decode.succeed ConstTypeFacet

        "DocumentationKind" ->
            Decode.succeed DocKind

        _ ->
            Decode.fail ("No such field: " ++ str)


documentationKindFromString : String -> Decoder DocumentationKind
documentationKindFromString str =
    case str of
        "Meta" ->
            Decode.succeed Meta

        "Inline" ->
            Decode.succeed Inline

        "Latex" ->
            Decode.succeed Latex

        _ ->
            Decode.fail ("No such documentation kind: " ++ str)



-- JSON ENCODINGS


encodeFilterTerm : FilterTerm -> Value
encodeFilterTerm term =
    case term of
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

        AnyInResult fq ->
            object [ ( "AnyInResult", encodeAbstractFQ fq ) ]

        AllInResult fq ->
            object [ ( "AllInResult", encodeAbstractFQ fq ) ]


encodeAbstractFQ : AbstractFQ -> Value
encodeAbstractFQ fq =
    case fq of
        Filter filterFields ->
            object
                [ ( "Filter"
                  , object
                        [ ( "fieldTerms", object (List.map encodeFieldTerm filterFields) ) ]
                  )
                ]

        Intersection fq1 fq2 fqs ->
            object
                [ ( "FilterIntersection"
                  , object
                        [ ( "f1", encodeAbstractFQ fq1 )
                        , ( "f2", encodeAbstractFQ fq2 )
                        , ( "fn", Encode.list encodeAbstractFQ fqs )
                        ]
                  )
                ]

        Union fq1 fq2 fqs ->
            object
                [ ( "FilterUnion"
                  , object
                        [ ( "f1", encodeAbstractFQ fq1 )
                        , ( "f2", encodeAbstractFQ fq2 )
                        , ( "fn", Encode.list encodeAbstractFQ fqs )
                        ]
                  )
                ]

        Complement fq1 ->
            object [ ( "FilterComplement", object [ ( "filter", encodeAbstractFQ fq1 ) ] ) ]


encodeFieldTerm : ( Field, FilterTerm ) -> ( String, Value )
encodeFieldTerm ( field, term ) =
    ( fieldToString field, encodeFilterTerm term )


encodeFilterQuery : FilterQuery -> Value
encodeFilterQuery filterQuery =
    [ Just ( "filter", encodeAbstractFQ filterQuery.filter )
    , Just ( "pageSize", Encode.int filterQuery.pageSize )
    , filterQuery.cursor |> Maybe.map (\c -> ( "cursor", Encode.string c ))
    ]
        |> Maybe.Extra.values
        |> object


encodeFacetQuery : FacetQuery -> Value
encodeFacetQuery facetQuery =
    object
        [ ( "filter", encodeAbstractFQ facetQuery.filter )
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
    Decode.string |> Decode.andThen kindFromString


documentationKindDecoder : Decoder DocumentationKind
documentationKindDecoder =
    Decode.string |> Decode.andThen documentationKindFromString


shortCmdDecoder : Decoder ShortCmd
shortCmdDecoder =
    Decode.oneOf [ Decode.map Block shortBlockDecoder, Decode.map Doc documentationDecoder ]


documentationDecoder : Decoder Documentation
documentationDecoder =
    Decode.map4 Documentation
        (Decode.field "id" Decode.string)
        (Decode.field "theory" Decode.string)
        (Decode.field "src" Decode.string)
        (Decode.field "docKind" (Decode.string |> Decode.andThen documentationKindFromString))


shortBlockDecoder : Decoder ShortBlock
shortBlockDecoder =
    Decode.map4 ShortBlock
        (Decode.field "id" Decode.string)
        (Decode.field "theory" Decode.string)
        (Decode.field "src" Decode.string)
        (Decode.field "entities" (Decode.list shortEtDecoder))


shortEtDecoder : Decoder ShortEt
shortEtDecoder =
    Decode.map3 ShortEt
        (Decode.field "id" Decode.string)
        (Decode.field "kind" kindDecoder)
        (Decode.field "name" Decode.string)


facetDecoder : Decoder Facet
facetDecoder =
    Decode.dict Decode.int


fieldDecoder : Decoder Field
fieldDecoder =
    Decode.string |> Decode.andThen fieldFromString


facetResultDecoder : Decoder FacetResult
facetResultDecoder =
    anyDictDecoder fieldFromString facetDecoder fieldToString


constantEtDecoder : Decoder ConstantEt
constantEtDecoder =
    Decode.map4 ConstantEt
        (Decode.field "id" Decode.string)
        (Decode.field "typ" Decode.string)
        (Decode.field "typUses" (Decode.list shortEtDecoder))
        (Decode.field "propUses" (Decode.list shortEtDecoder))


factEtDecoder : Decoder FactEt
factEtDecoder =
    Decode.map3 FactEt
        (Decode.field "id" Decode.string)
        (Decode.field "propUses" (Decode.list shortEtDecoder))
        (Decode.field "proofUses" (Decode.list shortEtDecoder))


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
    Decode.oneOf
        [ Decode.map ThyConstant constantEtDecoder
        , Decode.map ThyFact factEtDecoder
        , Decode.map ThyType typeEtDecoder
        ]
