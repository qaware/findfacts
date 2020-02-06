module Results exposing (Config, State, init, view)

import Array exposing (Array)
import Array.Extra
import Bootstrap.Badge as Badge
import Bootstrap.Button as Button
import Bootstrap.Card as Card
import Bootstrap.Card.Block as Block
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Bootstrap.ListGroup as ListGroup
import Bootstrap.Text as Text
import Entities exposing (Kind(..), ResultShortlist, ShortBlock, ShortEt, compareByKind, kindToString)
import Html exposing (Html, br, pre, text)



-- CONFIG


type alias Config msg =
    State -> msg



-- STATE


type alias Result =
    { open : Bool
    , block : ShortBlock
    }


type alias State =
    Array Result


init : ResultShortlist -> State
init resultList =
    resultList.values |> List.map (Result False) |> Array.fromList



-- VIEW


view : Config msg -> State -> List (Html msg)
view config state =
    state
        |> Array.indexedMap (\i res -> renderResult config res (\newRes -> Array.Extra.update i (\_ -> newRes) state))
        |> Array.toList
        |> List.concat


renderResult : Config msg -> Result -> (Result -> State) -> List (Html msg)
renderResult conf res updateState =
    [ Card.config [ Card.align Text.alignXsLeft, Card.outlineSecondary ]
        |> Card.header [] []
        |> Card.block [ Block.textColor Text.secondary ]
            [ Block.text [] [ pre [] [ text res.block.src ] ]
            ]
        -- Render entity list if necessary
        |> Card.listGroup
            (if res.open then
                ListGroup.li [] [] :: (res.block.entities |> List.sortBy compareByKind |> List.map renderEntity)

             else
                []
            )
        -- Render summary if necessary
        |> Card.block [ Block.align Text.alignXsLeft ]
            (if List.isEmpty res.block.entities || res.open then
                []

             else
                [ renderEntitySummary res.block.entities ]
            )
        -- Render open/close buttons if necessary
        |> Card.block [ Block.align Text.alignXsRight ]
            (if List.isEmpty res.block.entities then
                []

             else
                [ Block.custom <|
                    Button.button
                        [ Button.secondary
                        , Button.small
                        , Button.onClick ({ res | open = not res.open } |> updateState |> conf)
                        ]
                        [ text
                            (if res.open then
                                "show less"

                             else
                                "show more"
                            )
                        ]
                ]
            )
        |> Card.footer [] [ text res.block.file ]
        |> Card.view
    , br [] []
    ]


renderEntitySummary : List ShortEt -> Block.Item msg
renderEntitySummary blocks =
    let
        counts =
            ( "Types", List.filter (\et -> et.kind == Type) blocks |> List.length )
                :: ( "Constants", List.filter (\et -> et.kind == Constant) blocks |> List.length )
                :: ( "Facts", List.filter (\et -> et.kind == Fact) blocks |> List.length )
                :: []

        cols =
            counts
                |> List.filter (\( _, c ) -> c > 0)
                |> List.map
                    (\( name, count ) ->
                        Grid.col [ Col.xsAuto ]
                            [ Badge.badgePrimary [] [ text (name ++ ": " ++ String.fromInt count) ] ]
                    )
    in
    Block.custom <| Grid.container [] [ Grid.row [] cols ]


renderEntity : ShortEt -> ListGroup.Item msg
renderEntity et =
    ListGroup.li [ ListGroup.light ]
        [ Grid.container []
            (Grid.row []
                [ Grid.col [ Col.xs, Col.lg2 ] [ text (kindToString et.kind) ]
                , Grid.col [] [ text et.name ]
                ]
                :: (if String.isEmpty et.shortDescription then
                        []

                    else
                        [ Grid.row []
                            [ Grid.col [ Col.xs, Col.lg2 ] []
                            , Grid.col [] [ text et.shortDescription ]
                            ]
                        ]
                   )
            )
        ]
