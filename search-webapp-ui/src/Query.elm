module Query exposing (AbstractFQ(..), Facet, FacetResult, Field(..), FilterTerm(..), Query(..), decode, encode, fieldToString)

import Dict exposing (Dict)
import Dict.Any as AnyDict exposing (AnyDict)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value, int, list, object, string)
import List exposing (map)
import Result.Extra


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


type Query
    = FilterQuery AbstractFQ Int
    | FacetQuery AbstractFQ (List Field) Int


type alias Facet =
    Dict String Int


type alias FacetResult =
    AnyDict String Field Facet



-- STRING


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



-- FROMSTRING


fieldFromString : String -> Result String Field
fieldFromString str =
    case str of
        "Id" ->
            Ok Id

        "CommandKind" ->
            Ok CmdKind

        "SourceText" ->
            Ok Src

        "SourceTheory" ->
            Ok SrcFile

        "Kind" ->
            Ok Kind

        "Name" ->
            Ok Name

        "Proposition" ->
            Ok Prop

        "ConstantType" ->
            Ok ConstType

        "ConstantTypeFacet" ->
            Ok ConstTypeFacet

        "DocumentationKind" ->
            Ok DocKind

        _ ->
            Err ("No such field: " ++ str)



-- JSON


encodeFilterTerm : FilterTerm -> Value
encodeFilterTerm term =
    case term of
        Term str ->
            object [ ( "Term", object [ ( "inner", string str ) ] ) ]

        Exact str ->
            object [ ( "Exact", object [ ( "inner", string str ) ] ) ]

        InRange from to ->
            object
                [ ( "InRange"
                  , object
                        [ ( "from", int from )
                        , ( "to", int to )
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
                        [ ( "fieldTerms", object (map encodeFieldTerm filterFields) ) ]
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


encode : Query -> Value
encode query =
    case query of
        FilterQuery filter maxResults ->
            object [ ( "filter", encodeAbstractFQ filter ), ( "maxResults", int maxResults ) ]

        FacetQuery filter fields maxFacets ->
            object
                [ ( "filter", encodeAbstractFQ filter )
                , ( "fields", list (\f -> f |> fieldToString |> string) fields )
                , ( "maxFacets", int maxFacets )
                ]


facetDecode : Decoder Facet
facetDecode =
    Decode.dict Decode.int


toFieldAnyDict : Dict String Facet -> Decoder (AnyDict String Field Facet)
toFieldAnyDict untypedDict =
    let
        typedRes =
            Dict.toList untypedDict
                |> List.map (\( fieldStr, facet ) -> fieldFromString fieldStr |> Result.map (\x -> ( x, facet )))
    in
    case Result.Extra.combine typedRes of
        Ok res ->
            Decode.succeed (AnyDict.fromList fieldToString res)

        Err e ->
            Decode.fail e


decode : Decoder FacetResult
decode =
    Decode.dict facetDecode |> Decode.andThen toFieldAnyDict
