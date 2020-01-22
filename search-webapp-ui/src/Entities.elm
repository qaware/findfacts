module Entities exposing (ResultList, decoder, kindToString, view)

import Bootstrap.Card as Card
import Bootstrap.Card.Block as Block
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Bootstrap.ListGroup as ListGroup
import Bootstrap.Text as Text
import Html exposing (Html, br, pre, text)
import Json.Decode as Decode exposing (Decoder, list)
import List exposing (map)



-- Entities


type alias ResultList =
    List ShortBlock


type alias ShortBlock =
    { id : String
    , file : String
    , src : String
    , entities : List ShortEt
    }


type alias ShortEt =
    { id : String
    , kind : Kind
    , name : String
    , proposition : String
    , shortDescription : String
    }


type Kind
    = Constant
    | Documentation
    | Fact
    | Type



-- STRING


kindToString : Kind -> String
kindToString kind =
    case kind of
        Constant ->
            "Constant"

        Documentation ->
            "Documentation"

        Fact ->
            "Fact"

        Type ->
            "Type"



-- JSON


decoder : Decoder ResultList
decoder =
    Decode.list blockDecoder


kindDecoder : Decoder Kind
kindDecoder =
    Decode.string |> Decode.andThen kindFromString


kindFromString : String -> Decoder Kind
kindFromString string =
    case string of
        "Constant" ->
            Decode.succeed Constant

        "Documentation" ->
            Decode.succeed Documentation

        "Fact" ->
            Decode.succeed Fact

        "Type" ->
            Decode.succeed Type

        _ ->
            Decode.fail ("Invalid kind: " ++ string)


blockDecoder : Decoder ShortBlock
blockDecoder =
    Decode.map4 ShortBlock
        (Decode.field "id" Decode.string)
        (Decode.field "file" Decode.string)
        (Decode.field "src" Decode.string)
        (Decode.field "entities" childDecoder)


childDecoder : Decoder (List ShortEt)
childDecoder =
    list shortDecoder


shortDecoder : Decoder ShortEt
shortDecoder =
    Decode.map5 ShortEt
        (Decode.field "id" Decode.string)
        (Decode.field "kind" kindDecoder)
        (Decode.field "name" Decode.string)
        (Decode.field "proposition" Decode.string)
        (Decode.field "description" Decode.string)



-- VIEW


view : ResultList -> List (Html msg)
view res =
    List.indexedMap Tuple.pair res
        |> map renderResult
        |> List.concat


renderResult : ( Int, ShortBlock ) -> List (Html msg)
renderResult ( _, res ) =
    [ Card.config [ Card.align Text.alignXsLeft, Card.outlineSecondary ]
        |> Card.header [] []
        |> Card.block [ Block.textColor Text.secondary ]
            [ Block.text [] [ pre [] [ text res.src ] ]
            ]
        |> Card.listGroup (ListGroup.li [] [] :: map renderEntity res.entities)
        |> Card.footer [] [ text res.file ]
        |> Card.view
    , br [] []
    ]


renderEntity : ShortEt -> ListGroup.Item msg
renderEntity et =
    ListGroup.li [ ListGroup.light ]
        [ Grid.container []
            [ Grid.row []
                [ Grid.col [ Col.xs, Col.lg2 ] [ text (kindToString et.kind) ]
                , Grid.col [] [ text et.name ]
                ]
            , Grid.row []
                [ Grid.col [ Col.xs, Col.lg2 ] []
                , Grid.col [] [ text et.proposition ]
                ]
            ]
        ]
