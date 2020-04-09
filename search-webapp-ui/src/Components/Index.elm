module Components.Index exposing
    ( Config, State
    , config, withAdditional, init, view
    , select, selected
    , update
    )

{-| Module for index selection.


# Types

@docs Config, State


# Component

@docs config, withAdditional, empty, init, view


# Helpers

@docs select, selected

-}

import Html exposing (Html, text)
import Material.Select as Select exposing (selectConfig, selectOptionConfig)


separator =
    "_"


{-| Opaque config type.
-}
type Config msg
    = Config (ConfigInternal msg)


type alias ConfigInternal msg =
    { toMsg : State -> msg
    , additionalAttrs : List (Html.Attribute msg)
    }


{-| Creates a new config.
-}
config : (State -> msg) -> Config msg
config toSelect =
    Config <| ConfigInternal toSelect []


{-| Adds additional attributes to the index selector element.
-}
withAdditional : List (Html.Attribute msg) -> Config msg -> Config msg
withAdditional attrs (Config conf) =
    Config { conf | additionalAttrs = attrs }


{-| Opaque state type.
-}
type State
    = State StateInternal


type alias StateInternal =
    { status : Status
    , selected : Index
    }


type Status
    = Fetching
    | Elems (List Index)


type alias Index =
    { string : String
    , parsed : Maybe NamedIndex
    }


type alias NamedIndex =
    { isabelleVersion : String
    , afpVersion : String
    , name : String
    }


{-| Creates an initial state from a current selection (but without alternatives).
-}
init : String -> State
init default =
    State (StateInternal Fetching <| fromString default)


{-| Updates a state with options.
-}
update : List String -> State -> State
update indexes (State state) =
    State { state | status = Elems (List.map fromString indexes) }


{-| Updates a state with a selection.
-}
select : String -> State -> State
select index (State state) =
    State <| { state | selected = fromString index }


{-| Index name schema: ${NAME}\_${ISABELLE\_COMMIT\_HASH}_${AFP_COMMIT\_HASH}
-}
fromString : String -> Index
fromString s =
    let
        parsed =
            case String.split separator s of
                [ name, isabelle, afp ] ->
                    Just <| NamedIndex isabelle afp name

                _ ->
                    Nothing
    in
    Index s parsed


{-| Gets the selected element.
-}
selected : State -> String
selected (State state) =
    state.selected.string


{-| Renders the index selector.
-}
view : State -> Config msg -> Html msg
view (State state) (Config conf) =
    case state.status of
        Fetching ->
            Select.filledSelect
                { selectConfig
                    | label = "Fetching..."
                    , disabled = True
                    , value = Just <| state.selected.string
                    , additionalAttributes = conf.additionalAttrs
                }
                -- This option must be here, otherwise a visual bug can occur
                [ Select.selectOption { selectOptionConfig | value = state.selected.string }
                    [ text <| display state.selected ]
                ]

        Elems elems ->
            Select.filledSelect
                { selectConfig
                    | label = "Index"
                    , value = Just <| state.selected.string
                    , onChange = Just <| \s -> conf.toMsg <| State <| { state | selected = fromString s }
                    , additionalAttributes = conf.additionalAttrs
                }
                (List.map
                    (\index ->
                        Select.selectOption { selectOptionConfig | value = index.string }
                            [ text <| display index ]
                    )
                    elems
                )


display : Index -> String
display index =
    index.parsed
        |> Maybe.map (\named -> named.name ++ " (" ++ named.isabelleVersion ++ " / " ++ named.afpVersion ++ ")")
        |> Maybe.withDefault index.string
