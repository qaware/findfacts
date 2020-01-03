module Query exposing (Query, QueryField, encode, fromString)

import Json.Decode as D
import Json.Encode as E



type alias Query =
    List QueryField


type QueryField
    = Kind String
    | Name String
    | Uses Query


-- String


fromString : String -> Result String Query
fromString _ =
    Err "Not implemented"



-- JSON


encode : Query -> E.Value
encode query =
    E.object (List.map encodeQueryField query)


encodeQueryField : QueryField -> ( String, E.Value )
encodeQueryField field =
    case field of
        Kind k ->
            ( "kind", E.string k )

        Name n ->
            ( "name", E.string n )

        Uses query ->
            ( "uses", encode query )