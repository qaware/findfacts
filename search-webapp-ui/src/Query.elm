module Query exposing (Field, FilterQuery, encode, fromString)

import Json.Encode exposing (Value, int, object, string)
import List exposing (map)


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
fromString _ =
    -- TODO
    Ok (FilterQuery (Filter [ ( Name, StringExpression "*gauss*" ) ]) 100)


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
