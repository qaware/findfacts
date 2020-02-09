module PagingComponent exposing (Config, State, buildFilterQuery, cursorParser, init, view)

import Html exposing (Html)
import List.Extra as ListExtra
import Query exposing (AbstractFQ, FilterQuery)
import Url.Parser as UrlParser



-- CONFIG


pageSize =
    20


type alias Config msg =
    State -> msg



-- STATE


type alias State =
    { pageCursors : List String
    , totalResults : Int
    }


init : State
init =
    State [] 0



-- ENCODING/DECODING


stateFromString : String -> State
stateFromString s =
    -- TODO
    init


cursorParser : UrlParser.Parser (State -> a) a
cursorParser =
    UrlParser.map stateFromString UrlParser.string



-- QUERYING


buildFilterQuery : AbstractFQ -> State -> FilterQuery
buildFilterQuery fq state =
    FilterQuery fq pageSize (ListExtra.last state.pageCursors)



-- VIEW


view : Config msg -> State -> List (Html msg)
view cfg state =
    -- TODO
    []
