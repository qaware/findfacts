module Query exposing (Field, FilterQuery, encode, fromString)

import Json.Encode exposing (Value, int, object, string)
import List exposing (foldr, map)


type FilterTerm
    = Id String
    | Number Int
    | StringExpression String
    | InRange Int Int
    | AnyInResult AbstractFQ
    | AllInResult AbstractFQ


type AbstractFQ
    = Filter (List ( Field, FilterTerm ))
    | Intersection (List AbstractFQ)
    | Union (List AbstractFQ)
    | Complement AbstractFQ


type Field
    = Name
    | Kind
    | StartPosition
    | EndPosition


type alias FilterQuery =
    { filter : AbstractFQ
    , maxResults : Int
    }


type alias FacetQuery =
    { filter : AbstractFQ
    , field : String
    }



-- String


fromString : String -> Result String FilterQuery
fromString str =
    if str == "" then
        Err "No query"

    else
        let
            filterTermResults =
                map filterTermFromString (String.split " " str)

            filterTerms =
                foldr
                    (\term acc ->
                        case ( term, acc ) of
                            ( Ok t, Ok ts ) ->
                                Ok (t :: ts)

                            ( Err e, Ok ts ) ->
                                Err e

                            _ ->
                                acc
                    )
                    (Ok [])
                    filterTermResults
        in
        Result.map (\terms -> FilterQuery (Filter terms) 10) filterTerms


filterTermFromString : String -> Result String ( Field, FilterTerm )
filterTermFromString str =
    case String.split ":" str of
        [ field, filter ] ->
            if String.isEmpty filter then
                Err "Empty filter"

            else
                Result.map (\f -> ( f, StringExpression filter )) (fieldFromString field)

        _ ->
            Err "Invalid query"


fieldFromString : String -> Result String Field
fieldFromString str =
    case str of
        "Name" ->
            Ok Name

        "Kind" ->
            Ok Kind

        "StartPosition" ->
            Ok StartPosition

        "EndPosition" ->
            Ok EndPosition

        _ ->
            Err ("No such field: " ++ str)


fieldToString : Field -> String
fieldToString field =
    case field of
        Name ->
            "Name"

        Kind ->
            "Kind"

        StartPosition ->
            "StartPosition"

        EndPosition ->
            "EndPosition"



-- JSON


encodeFilterTerm : FilterTerm -> Value
encodeFilterTerm term =
    case term of
        Id id ->
            object [ ( "Id", object [ ( "inner", string id ) ] ) ]

        Number num ->
            object [ ( "Number", object [ ( "inner", int num ) ] ) ]

        StringExpression str ->
            object [ ( "StringExpression", object [ ( "inner", string str ) ] ) ]

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

        Intersection fqs ->
            string "TODO"

        Union fqs ->
            string "TODO"

        Complement fq1 ->
            string "TODO"


encodeFieldTerm : ( Field, FilterTerm ) -> ( String, Value )
encodeFieldTerm ( field, term ) =
    ( fieldToString field, encodeFilterTerm term )


encode : FilterQuery -> Value
encode query =
    object
        [ ( "FilterQuery"
          , object
                [ ( "filter", encodeAbstractFQ query.filter )
                , ( "maxResults", int query.maxResults )
                ]
          )
        ]
