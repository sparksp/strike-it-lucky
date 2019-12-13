module Lucky exposing (Model, Msg, init, update, view)

import Array exposing (Array)
import Difficulty exposing (Difficulty)
import Html exposing (Html, a, div, h1, h2, span, text)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Random



--- MODEL


type Question
    = Single
    | Team


type Answer
    = Pass
    | Fail


type Tile
    = Loading
    | HotSpot
    | Question Question
    | Answer Answer


type Space
    = Top
    | Middle
    | Bottom


type alias Selection =
    { space : Space
    , tile : Tile
    }


type alias Board =
    Array (Maybe Selection)


type alias Player r =
    { r
        | board : Board
        , step : Int
        , playerName : String
    }


type alias Model =
    { difficulty : Difficulty
    , streak : Int
    , space : Maybe Space
    , board : Board
    , step : Int
    , playerName : String
    }


initialModel : Model
initialModel =
    { step = 0
    , streak = 0
    , space = Nothing
    , board = Array.repeat 9 Nothing
    , difficulty = Difficulty.fromInt 4 |> Maybe.withDefault Difficulty.min
    , playerName = "Player 1"
    }


init : ( Model, Cmd Msg )
init =
    ( initialModel, Cmd.none )


getCurrentSelection : Int -> Board -> Maybe Selection
getCurrentSelection step board =
    Array.get step board
        |> Maybe.withDefault Nothing



--- COMMANDS


randomTile : Difficulty -> Int -> Random.Generator Tile
randomTile difficulty streak =
    Random.weighted
        ( 15, Question Single )
        [ ( 4, Question Team )
        , ( toFloat ((Difficulty.toInt difficulty * streak) + 2), HotSpot )
        , ( 4, Answer Pass )
        ]


newTile : Difficulty -> Int -> Cmd Msg
newTile difficulty streak =
    Random.generate NewTile (randomTile difficulty streak)



--- UPDATE


type Msg
    = Select Space
    | NewTile Tile
    | NewAnswer Answer
    | Reset


selectSpace : Int -> Selection -> Board -> Board
selectSpace step selection board =
    Array.set step (Just selection) board


updateStep : Tile -> Int -> Int
updateStep tile step =
    case tile of
        Answer Pass ->
            step + 1

        _ ->
            step


updateSpace : Tile -> Space -> Maybe Space
updateSpace tile currentSpace =
    case tile of
        Answer _ ->
            Nothing

        HotSpot ->
            Nothing

        _ ->
            Just currentSpace


updateStreak : Tile -> Int -> Int
updateStreak tile streak =
    case tile of
        Answer Pass ->
            streak + 1

        Answer Fail ->
            0

        HotSpot ->
            0

        _ ->
            streak


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model.space ) of
        ( Reset, _ ) ->
            init

        ( Select space, Nothing ) ->
            ( { model
                | board = selectSpace model.step (Selection space Loading) model.board
                , space = Just space
              }
            , newTile model.difficulty model.streak
            )

        ( NewAnswer answer, Just space ) ->
            let
                tile =
                    Answer answer
            in
            ( { model
                | board = selectSpace model.step (Selection space tile) model.board
                , step = updateStep tile model.step
                , space = updateSpace tile space
                , streak = updateStreak tile model.streak
              }
            , Cmd.none
            )

        ( NewTile tile, Just space ) ->
            ( { model
                | board = selectSpace model.step (Selection space tile) model.board
                , step = updateStep tile model.step
                , space = updateSpace tile space
                , streak = updateStreak tile model.streak
              }
            , Cmd.none
            )

        ( _, _ ) ->
            ( model, Cmd.none )



--- VIEW


tileToString : Maybe Tile -> String
tileToString tile =
    case tile of
        Just (Question Single) ->
            "Q"

        Just (Question Team) ->
            "QQ"

        Just HotSpot ->
            "🔥"

        Just (Answer Pass) ->
            ">>>"

        Just (Answer Fail) ->
            "❌"

        Just Loading ->
            ""

        Nothing ->
            "?"


tileToClass : Maybe Tile -> String
tileToClass tile =
    case tile of
        Just (Question Single) ->
            "tile-question"

        Just (Question Team) ->
            "tile-question"

        Just HotSpot ->
            "tile-fail"

        Just (Answer Pass) ->
            "tile-pass"

        Just (Answer Fail) ->
            "tile-fail"

        Just Loading ->
            "tile-loading"

        Nothing ->
            "tile-blank"


viewTile : Maybe Tile -> Html Msg
viewTile tile =
    div [ class "tile", class (tileToClass tile) ]
        [ text (tileToString tile) ]


viewTileWithAction : Space -> Maybe Tile -> Html Msg
viewTileWithAction space tile =
    div
        [ class "tile tile-action"
        , class (tileToClass tile)
        , onClick (Select space)
        ]
        [ text (tileToString tile) ]


filterSelectionBySpace : Space -> Selection -> Maybe Selection
filterSelectionBySpace currentSpace selection =
    if currentSpace == selection.space then
        Just selection

    else
        Nothing


viewTileSpace : (Maybe Tile -> Html Msg) -> Space -> Maybe Selection -> Html Msg
viewTileSpace viewer currentSpace selection =
    selection
        |> Maybe.andThen (filterSelectionBySpace currentSpace)
        |> Maybe.map .tile
        |> viewer


selectionIsQuestion : Selection -> Bool
selectionIsQuestion { tile } =
    case tile of
        Question _ ->
            True

        _ ->
            False


canSelectSpace : Maybe Selection -> Int -> Int -> Bool
canSelectSpace currentSelection currentStep step =
    if
        currentSelection
            |> Maybe.map selectionIsQuestion
            |> Maybe.withDefault False
    then
        False

    else
        currentStep < 9 && currentStep == step


viewSpaceTiles : Bool -> Maybe Selection -> Html Msg
viewSpaceTiles active selection =
    div [ class "space" ]
        (if active then
            [ viewTileSpace (viewTileWithAction Top) Top selection
            , viewTileSpace (viewTileWithAction Middle) Middle selection
            , viewTileSpace (viewTileWithAction Bottom) Bottom selection
            ]

         else
            [ viewTileSpace viewTile Top selection
            , viewTileSpace viewTile Middle selection
            , viewTileSpace viewTile Bottom selection
            ]
        )


viewBoard : Player r -> Html Msg
viewBoard { playerName, step, board } =
    let
        currentSelection =
            getCurrentSelection step board

        currentCanSelectSpace =
            canSelectSpace currentSelection step
    in
    div [ class "active-player" ]
        [ h2 [ class "player-name" ] [ text playerName ]
        , board
            |> Array.indexedMap
                (\index selection ->
                    viewSpaceTiles (currentCanSelectSpace index) selection
                )
            |> Array.toList
            |> div [ class "board" ]
        ]



-- Mini Board


viewSpace : Maybe Selection -> Html Msg
viewSpace selection =
    div [ class "space" ]
        [ selection |> Maybe.map .tile |> viewTile ]


viewMiniBoard : Player r -> Html Msg
viewMiniBoard { playerName, board } =
    div [ class "player mini-board" ]
        [ h2 [ class "player-name" ] [ text playerName ]
        , board
            |> Array.map viewSpace
            |> Array.toList
            |> div [ class "board" ]
        ]


viewQuestionControls : Int -> Maybe Selection -> List (Html Msg)
viewQuestionControls step currentSelection =
    if
        currentSelection
            |> Maybe.map selectionIsQuestion
            |> Maybe.withDefault False
    then
        -- Answer Question
        [ span [ class "label" ] [ text "Answer:" ]
        , a [ class "btn btn-pass", onClick (NewAnswer Pass) ] [ text "Correct" ]
        , a [ class "btn btn-fail", onClick (NewAnswer Fail) ] [ text "Wrong" ]
        ]

    else if step < 9 then
        -- Choose Space
        [ span [ class "label" ] [ text "Choose:" ]
        , a [ class "btn btn-space btn-top", onClick (Select Top) ] [ text "Top" ]
        , a [ class "btn btn-space btn-middle", onClick (Select Middle) ] [ text "Middle" ]
        , a [ class "btn btn-space btn-bottom", onClick (Select Bottom) ] [ text "Bottom" ]
        ]

    else
        -- Game Complete
        []


viewResetControl : Int -> Maybe Selection -> List (Html Msg)
viewResetControl step currentSelection =
    if step == 0 && currentSelection == Nothing then
        [ a [ class "btn btn-disabled" ] [ text "Reset" ] ]

    else
        [ a [ class "btn btn-reset", onClick Reset ] [ text "Reset" ] ]


viewControls : Player r -> Html Msg
viewControls { step, board } =
    let
        currentSelection =
            getCurrentSelection step board
    in
    div [ class "controls" ]
        (viewQuestionControls step currentSelection
            ++ [ span [ class "spacer" ] [] ]
            ++ viewResetControl step currentSelection
        )


view : Player r -> Html Msg
view player =
    div [ class "main" ]
        [ h1 [] [ text "Strike It Lucky" ]
        , viewBoard player
        , viewControls player
        , viewMiniBoard player
        ]
