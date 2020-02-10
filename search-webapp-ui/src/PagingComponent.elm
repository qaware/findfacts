module PagingComponent exposing (Config, State, buildFilterQuery, decoder, empty, encode, update, view)

import Entities exposing (ResultShortlist)
import Html exposing (Html)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)
import Query exposing (AbstractFQ, FilterQuery)



-- CONFIG


pageSize =
    20


type alias Config msg =
    State -> msg



-- STATE


type alias State =
    { -- Next cursor is not set when query was executed but result is not there yet
      nextCursor : Maybe String
    , previous : List String
    }


empty : State
empty =
    State Nothing []


update : ResultShortlist -> State -> State
update res state =
    { state | nextCursor = Just res.nextCursor }



-- ENCODING


encode : State -> Value
encode state =
    Encode.list Encode.string state.previous



--DECODING


decoder : Decoder State
decoder =
    Decode.map (State Nothing) (Decode.list Decode.string)



-- QUERYING


buildFilterQuery : AbstractFQ -> State -> FilterQuery
buildFilterQuery fq state =
    FilterQuery fq pageSize state.nextCursor



-- VIEW


view : Config msg -> State -> List (Html msg)
view cfg state =
    -- TODO
    []
