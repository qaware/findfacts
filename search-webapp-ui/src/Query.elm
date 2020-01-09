module Query exposing (Field, FilterQuery, encode, fromString)

import Json.Encode exposing (Value, int, object, string)


type FilterTerm
    = Id String
    | Number Int
    | StringExpression String
    | InRange Int Int
    | AnyInResult AbstractFQ
    | AllInResult AbstractFQ


type AbstractFQ
    = Filter (Field -> FilterTerm)
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
    Ok (FilterQuery (Filter (\x -> Number 1)) 1)



-- JSON


encode : FilterQuery -> Value
encode query =
    object
        [ ( "FilterQuery"
          , object
                [ ( "filter"
                  , object
                        [ ( "Filter"
                          , object
                                [ ( "fieldTerms"
                                  , object
                                        [ ( "Name"
                                          , object
                                                [ ( "StringExpression"
                                                  , object
                                                        [ ( "inner", string "*gauss*" ) ]
                                                  )
                                                ]
                                          )
                                        ]
                                  )
                                ]
                          )
                        ]
                  )
                , ( "maxResults", int 10 )
                ]
          )
        ]
