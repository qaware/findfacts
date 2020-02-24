module PagingComponent exposing
    ( Config, State
    , config, empty, encode, decoder, update, view
    , buildFilterQuery
    , isEmpty, samePage, numResults
    )

{-| This component controls results pagination.


# Types

@docs Config, State


# Component

@docs config, empty, encode, decoder, update, view


# Special paging functionality

@docs buildFilterQuery


# Helpers

@docs isEmpty, samePage, numResults

-}

import Array exposing (Array)
import DataTypes exposing (..)
import Html exposing (Html, div, span, text)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)
import List.Extra
import Material.IconButton as IconButton exposing (iconButtonConfig)
import Material.LayoutGrid as Grid
import Material.Typography as Typography
import Maybe.Extra
import Util exposing (toMaybe)



-- CONFIG


{-| Opaque config type for the paging component.
-}
type Config msg
    = Config (State -> msg)


{-| Opaque state type for the paging component.
-}
type State
    = State StateInternal


type alias StateInternal =
    { -- Next cursor is not set when query was executed but result is not there yet
      next : Maybe String
    , totalResults : Int
    , previous : Array String
    }


{-| Creates a config for a paging component.
-}
config : (State -> msg) -> Config msg
config toMsg =
    Config toMsg


{-| Creates an initial state for a paging component.
-}
empty : State
empty =
    State (StateInternal Nothing 0 Array.empty)


{-| Encodes the persistent component state as json.
-}
encode : State -> Value
encode (State state) =
    Encode.array Encode.string state.previous


{-| Decodes a json of the persistent component state.
-}
decoder : Decoder State
decoder =
    Decode.array Decode.string
        |> Decode.map (StateInternal Nothing 0)
        |> Decode.map State


{-| Update state with new results.
-}
update : ResultList a -> State -> State
update res (State state) =
    State { state | next = Just res.nextCursor, totalResults = res.count }


{-| Renders the paging component.
-}
view : State -> Config msg -> Html msg
view (State state) (Config toMsg) =
    div [] <|
        if isEmpty (State state) then
            []

        else
            [ Grid.layoutGrid []
                [ Grid.layoutGridInner []
                    [ Grid.layoutGridCell [ Grid.alignMiddle, Typography.subtitle2 ]
                        (renderButtons
                            (ceiling (toFloat state.totalResults / pageSize))
                            (Array.length state.previous)
                            toMsg
                            state
                        )
                    ]
                ]
            ]


{-| Builds a correctly paged filter query from a filter and the paging component.
-}
buildFilterQuery : List FieldFilter -> State -> FilterQuery
buildFilterQuery fq (State state) =
    FilterQuery fq pageSize (state.previous |> Array.toList |> List.Extra.last)


{-| Checks if the paging is empty, i.e. does not have any results to control the page for.

    (PagingComponent.isEmpty PagingComponent.empty) == True
    -- for non-empty res
    (PagingComponent.isEmpty (PagingComponent.update res PagingComponent.empty) == False

-}
isEmpty : State -> Bool
isEmpty (State state) =
    state.totalResults == 0


{-| Checks if two components are on the same page.

    PagingComponent.samePage PagingComponent.empty (PagingComponent.update res PagingComponent.empty) == True

-}
samePage : State -> State -> Bool
samePage (State s1) (State s2) =
    s1.previous == s2.previous


{-| Gets the total number of results.
-}
numResults : State -> Int
numResults (State state) =
    state.totalResults



-- INTERNALS


pageSize =
    10


type ButtonState
    = Enabled StateInternal
    | Waiting


renderButtons : Int -> Int -> (State -> msg) -> StateInternal -> List (Html msg)
renderButtons numPages numPrevious conf state =
    Maybe.Extra.values
        [ Just <| Grid.layoutGridCell [] []

        -- previous
        , (numPrevious > 0)
            |> toMaybe (Array.get (numPrevious - 1) state.previous)
            |> Maybe.Extra.join
            |> Maybe.map
                (\c ->
                    IconButton.iconButton
                        { iconButtonConfig
                            | onClick =
                                Just <|
                                    conf <|
                                        State
                                            { state
                                                | previous = Array.slice 0 (numPrevious - 1) state.previous
                                                , next = Just c
                                            }
                        }
                        "navigate_before"
                )
        , -- current
          Just <| text <| String.fromInt <| numPrevious + 1
        , -- loading/next
          (numPrevious + 1 < numPages)
            |> toMaybe state.next
            |> Maybe.Extra.join
            |> Maybe.map
                (\c ->
                    IconButton.iconButton
                        { iconButtonConfig
                            | onClick =
                                Just <| conf <| State { state | previous = Array.push c state.previous, next = Nothing }
                        }
                        "navigate_next"
                )
        ]
