module ResultsComponent exposing
    ( Config, State
    , config, empty, searching, init, update, view
    , hasResults
    )

{-| This component displays results.


# Types

@docs Config, State


# Component

@docs config, empty, searching, init, update, view


# Helpers

@docs nonEmpty

-}

import Bootstrap.Badge as Badge
import Bootstrap.Button as Button
import Bootstrap.Card as Card
import Bootstrap.Card.Block as Block
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Bootstrap.ListGroup as ListGroup
import Bootstrap.Spinner as Spinner
import Bootstrap.Text as Text
import DataTypes exposing (..)
import Dict exposing (Dict)
import Html exposing (Html, br, div, hr, node, pre, text)
import Html.Attributes exposing (attribute, class, style, width)
import Html.Parser
import Html.Parser.Util
import Material.Button as MButton exposing (buttonConfig)
import Material.Card as MCard exposing (cardPrimaryActionConfig)
import Material.Chips as Chips
import Material.LayoutGrid as MGrid
import Material.Typography as Typography
import Maybe.Extra
import Util exposing (ite, pairWith, singletonIf, toMaybe)


{-| Opaque config type for the results component.
-}
type Config msg
    = Config (ConfigInternal msg)


type alias ConfigInternal msg =
    { toMsg : State -> msg
    , toDetailMsg : String -> msg
    , toUsageMsg : List String -> msg
    }


{-| Opaque state type for the results component.
-}
type State
    = Empty
    | Searching
    | Error String
    | Values StateInternal


type alias StateInternal =
    { blocks : List ShortCmd
    , blockState : Dict String Bool
    , entityState : Dict String EntityState
    }


type alias EntityState =
    { open : Bool
    , detail : Maybe ThyEt
    }


{-| Creates a config for a results component.
-}
config : (State -> msg) -> (String -> msg) -> (List String -> msg) -> Config msg
config toMsg toDetailMsg toUsageMsg =
    Config (ConfigInternal toMsg toDetailMsg toUsageMsg)


{-| Creates an initial empty state for the results component.
-}
empty : State
empty =
    Empty


{-| Creates an initial searching state for the results component.
-}
searching : State
searching =
    Searching


{-| Initializes a new state with the given results. Not an update function since Old results can be discarded.
-}
init : Result String (ResultList ShortCmd) -> State
init result =
    case result of
        Ok resultList ->
            Values
                { blocks = resultList.values
                , blockState =
                    resultList.values
                        |> getBlocks
                        |> List.map .id
                        |> List.map (pairWith False)
                        |> Dict.fromList
                , entityState =
                    resultList.values
                        |> getBlocks
                        |> List.concatMap .entities
                        |> List.map .id
                        |> List.map (pairWith (EntityState False Nothing))
                        |> Dict.fromList
                }

        Err cause ->
            Error cause


{-| Updates the state with a detail result.
-}
update : Result String ThyEt -> State -> State
update result state =
    case ( result, state ) of
        ( Ok res, Values vals ) ->
            let
                id =
                    case res of
                        ThyConstant constantEt ->
                            constantEt.id

                        ThyFact factEt ->
                            factEt.id

                        ThyType typeEt ->
                            typeEt.id
            in
            updateEntityDetail id res vals

        ( Ok _, _ ) ->
            state

        ( Err e, _ ) ->
            Error e


{-| Renders the results component.
-}
view : State -> Config msg -> List (Html msg)
view state (Config conf) =
    case state of
        Empty ->
            []

        Searching ->
            [ Spinner.spinner [] [] ]

        Error err ->
            [ text err ]

        Values res ->
            res.blocks
                |> List.map (renderCmd conf res)
                |> List.intersperse (br [] [])


{-| Checks if the results list is filed with results.
-}
hasResults : State -> Bool
hasResults state =
    case state of
        Values _ ->
            True

        _ ->
            False



-- INTERNALS
-- UPDATE


updateEntityDetail : String -> ThyEt -> StateInternal -> State
updateEntityDetail id thyEt state =
    Values { state | entityState = Dict.update id (\_ -> Just (EntityState True (Just thyEt))) state.entityState }


toggleBlockOpen : String -> StateInternal -> State
toggleBlockOpen id state =
    Values { state | blockState = Dict.update id (Maybe.map not) state.blockState }


closeEntity : String -> StateInternal -> State
closeEntity id state =
    Values { state | entityState = Dict.update id (Maybe.map (\es -> { es | open = False })) state.entityState }



-- HELPERS


getBlocks cmds =
    cmds
        |> List.filterMap
            (\cmd ->
                case cmd of
                    Block block ->
                        Just block

                    _ ->
                        Nothing
            )


toggleButton conf state label id =
    Button.button
        [ Button.secondary
        , Button.small
        , Button.onClick
            (state
                |> toggleBlockOpen id
                |> conf.toMsg
            )
        ]
        [ text label ]



-- RENDERING


renderCmd : ConfigInternal msg -> StateInternal -> ShortCmd -> Html msg
renderCmd conf state cmd =
    case cmd of
        Doc doc ->
            renderDocItem <| renderDocContent doc

        Block block ->
            renderBlockItem conf state block <| renderBlockContent conf block


renderDocItem : Html msg -> Html msg
renderDocItem content =
    MCard.card MCard.cardConfig
        { blocks = [ MCard.cardBlock <| MGrid.layoutGrid [ MGrid.alignLeft ] [ content ] ], actions = Nothing }


renderBlockItem : ConfigInternal msg -> StateInternal -> ShortBlock -> Html msg -> Html msg
renderBlockItem conf state block content =
    MCard.card MCard.cardConfig
        { blocks =
            MCard.cardPrimaryAction
                { cardPrimaryActionConfig | onClick = Just <| conf.toMsg <| toggleBlockOpen block.id state }
                [ MCard.cardBlock <| content ]
        , actions =
            Just <|
                MCard.cardActions { buttons = [ block.entities |> List.map .id |> renderFindUsageButton conf ], icons = [] }
        }


renderFindUsageButton conf ids =
    MCard.cardActionButton { buttonConfig | onClick = Just <| conf.toUsageMsg ids } "find usage"


renderHtml : String -> Html msg
renderHtml html =
    let
        renderedHtml =
            case Html.Parser.run html of
                Ok nodes ->
                    div [] <| Html.Parser.Util.toVirtualDom nodes

                _ ->
                    text html
    in
    pre [] [ renderedHtml ]


renderBlockContent : ConfigInternal msg -> ShortBlock -> Html msg
renderBlockContent conf block =
    MGrid.layoutGrid [ MGrid.alignLeft ]
        [ div [ Typography.caption, style "margin-bottom" "10pt" ] [ text block.file ]
        , renderHtml block.src
        , div [] <| renderSummaryBadges block.entities
        ]


renderSummaryChip ( name, count ) =
    count > 0 |> toMaybe (Chips.choiceChip Chips.choiceChipConfig <| name ++ ": " ++ String.fromInt count)


renderBadge label =
    Badge.pillSecondary [ style "margin-right" "10px", Typography.body2 ] [ text label ]


renderSummaryBadges ets =
    ( "Types", List.filter (\et -> et.kind == Type) ets |> List.length )
        :: ( "Constants", List.filter (\et -> et.kind == Constant) ets |> List.length )
        :: [ ( "Facts", List.filter (\et -> et.kind == Fact) ets |> List.length ) ]
        |> List.filter (\( _, c ) -> c > 0)
        |> List.map (\( name, count ) -> renderBadge (name ++ ": " ++ String.fromInt count))


renderBlockSummary : List ShortEt -> Html msg
renderBlockSummary ets =
    ( "Types", List.filter (\et -> et.kind == Type) ets |> List.length )
        :: ( "Constants", List.filter (\et -> et.kind == Constant) ets |> List.length )
        :: [ ( "Facts", List.filter (\et -> et.kind == Fact) ets |> List.length ) ]
        |> List.map renderSummaryChip
        |> Maybe.Extra.values
        |> Chips.choiceChipSet []


renderDocContent : Documentation -> Html msg
renderDocContent doc =
    div []
        [ div [ Typography.caption, style "margin-bottom" "10pt" ] [ text doc.file ]
        , renderHtml doc.src
        ]


renderDoc : Documentation -> List (Html msg)
renderDoc doc =
    [ Card.config [ Card.outlineSecondary ]
        |> Card.header [] [ text (documentationKindToString doc.docKind) ]
        |> Card.block [ Block.textColor Text.secondary ] [ Block.text [] [ pre [] [ text doc.src ] ] ]
        |> Card.footer [] [ text doc.file ]
        |> Card.view
    , br [] []
    ]


renderBlock : StateInternal -> ConfigInternal msg -> ShortBlock -> List (Html msg)
renderBlock state conf block =
    let
        open =
            Dict.get block.id state.blockState |> Maybe.withDefault False
    in
    [ Card.config [ Card.outlineSecondary ]
        |> Card.header [ class "text-right" ]
            (singletonIf
                (List.isEmpty block.entities |> not)
                (Button.button
                    [ Button.small, Button.onClick (conf.toUsageMsg (block.entities |> List.map .id)) ]
                    [ text "find usage" ]
                )
            )
        |> Card.block [ Block.textColor Text.secondary ]
            (Block.text [] [ pre [] [ text block.src ] ]
                :: singletonIf (not (open || List.isEmpty block.entities))
                    (Block.custom
                        (Grid.row [] [ Grid.col [] [ renderEntitySummary block.entities ] ])
                    )
            )
        |> (if open then
                -- render entity list if block is open
                Card.listGroup
                    -- Empty li as spacer
                    (ListGroup.li [ ListGroup.light ] []
                        :: (block.entities
                                |> List.sortBy (\a -> kindToString a.kind)
                                |> List.map (renderShortEt state conf)
                           )
                    )

            else
                identity
           )
        |> Card.footer []
            [ Grid.row []
                [ Grid.col [] [ text block.file ]
                , Grid.col [ Col.textAlign Text.alignXsRight ]
                    [ toggleButton conf state (ite open "show less" "show more") block.id ]
                ]
            ]
        |> Card.view
    , br [] []
    ]


renderEntitySummary : List ShortEt -> Html msg
renderEntitySummary blocks =
    let
        counts =
            ( "Types", List.filter (\et -> et.kind == Type) blocks |> List.length )
                :: ( "Constants", List.filter (\et -> et.kind == Constant) blocks |> List.length )
                :: [ ( "Facts", List.filter (\et -> et.kind == Fact) blocks |> List.length ) ]

        cols =
            counts
                |> List.filter (\( _, c ) -> c > 0)
                |> List.map
                    (\( name, count ) ->
                        Grid.col [ Col.xsAuto ]
                            [ Badge.badgePrimary [] [ text (name ++ ": " ++ String.fromInt count) ] ]
                    )
    in
    Grid.row [] cols


renderShortEt : StateInternal -> ConfigInternal msg -> ShortEt -> ListGroup.Item msg
renderShortEt state conf et =
    let
        ( open, detailMaybe ) =
            state.entityState
                |> Dict.get et.id
                |> Maybe.map (\s -> ( s.open, s.detail ))
                |> Maybe.withDefault ( False, Nothing )
    in
    ListGroup.li
        [ ListGroup.light ]
        [ Grid.container []
            ([ Grid.row []
                [ Grid.col [ Col.sm2 ] [ text (kindToString et.kind) ]
                , Grid.col [] [ text et.name ]
                , Grid.col [ Col.textAlign Text.alignXsRight, Col.sm2 ]
                    [ Button.button
                        [ Button.small
                        , Button.onClick
                            (ite open (conf.toMsg (state |> closeEntity et.id)) (conf.toDetailMsg et.id))
                        ]
                        [ text (ite open "close" "details") ]
                    ]
                , Grid.col [ Col.textAlign Text.alignXsRight, Col.sm2 ]
                    [ Button.button [ Button.small, Button.onClick (conf.toUsageMsg [ et.id ]) ] [ text "find usage" ] ]
                ]
             ]
                ++ (detailMaybe
                        |> Maybe.andThen (\detail -> open |> toMaybe (renderThyEt state conf detail))
                        |> Maybe.Extra.toList
                        |> List.concat
                   )
            )
        ]


renderThyEt : StateInternal -> ConfigInternal msg -> ThyEt -> List (Html msg)
renderThyEt state conf detail =
    case detail of
        ThyConstant const ->
            [ br [] [], pre [] [ text const.typ ] ]
                ++ renderEntitiesSummary "Type uses" const.typUses
                ++ renderEntitiesSummary "Proposition uses" const.propUses

        ThyFact fact ->
            br [] []
                :: renderEntitiesSummary "Proposition uses" fact.propUses
                ++ renderEntitiesSummary "Proof uses" fact.proofUses

        ThyType typ ->
            br [] []
                :: renderEntitiesSummary "Type uses" typ.ctorUses


renderEntitiesSummary : String -> List ShortEt -> List (Html msg)
renderEntitiesSummary name entities =
    ite (List.isEmpty entities)
        []
        [ Card.config []
            |> Card.header [] [ text name ]
            |> Card.listGroup
                (entities
                    |> List.map .name
                    |> List.map text
                    |> List.map List.singleton
                    |> List.map (ListGroup.li [])
                )
            |> Card.view
        , br [] []
        ]
