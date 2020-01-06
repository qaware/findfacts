module Query exposing (FilterQuery, Field, encode, fromString)

import Json.Encode as E


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
    { filter: AbstractFQ
    , maxResults: Int
    }

type alias FacetQuery =
    { filter: AbstractFQ
    , field: String
    }


-- String


fromString : String -> Result String FilterQuery
fromString _ =
    Err "Not implemented"



-- JSON


encode : FilterQuery -> E.Value
encode query = E.string "TODO"
