module Lucky exposing (Model, Msg, init, update, view)

import Array exposing (Array)
import Difficulty exposing (Difficulty)
import Html exposing (Html, a, div, h1, h2, span, text)
import Html.Attributes exposing (class, classList)
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
    | Question Question
    | Answer Answer


type Location
    = Top
    | Middle
    | Bottom


type alias Selection =
    { location : Location
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
    , board : Board
    , step : Int
    , playerName : String
    }


initialModel : Model
initialModel =
    { step = 0
    , streak = 0
    , board = Array.repeat 9 Nothing
    , difficulty = Difficulty.fromInt 4
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
        , ( toFloat ((Difficulty.toInt difficulty * streak) + 2), Answer Fail )
        , ( 4, Answer Pass )
        ]


newTile : Difficulty -> Int -> Cmd Msg
newTile difficulty streak =
    Random.generate NewTile (randomTile difficulty streak)



--- UPDATE


type Msg
    = Select Location
    | NewTile Tile
    | AnswerCorrect
    | AnswerWrong
    | TryAgain
    | Reset


clearLocation : Int -> Board -> Board
clearLocation step board =
    Array.set step Nothing board


selectLocation : Int -> Selection -> Board -> Board
selectLocation step selection board =
    Array.set step (Just selection) board


updateStep : Tile -> Int -> Int
updateStep tile step =
    case tile of
        Answer Pass ->
            step + 1

        _ ->
            step


updateStreak : Tile -> Int -> Int
updateStreak tile streak =
    case tile of
        Answer Pass ->
            streak + 1

        Answer Fail ->
            0

        _ ->
            streak


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        currentSelection =
            getCurrentSelection model.step model.board
    in
    case ( msg, currentSelection ) of
        ( Reset, _ ) ->
            init

        ( TryAgain, _ ) ->
            ( { model
                | board = clearLocation model.step model.board
                , streak = 0
              }
            , Cmd.none
            )

        ( Select location, Nothing ) ->
            ( { model
                | board = selectLocation model.step (Selection location Loading) model.board
              }
            , newTile model.difficulty model.streak
            )

        ( AnswerWrong, Just { location } ) ->
            ( { model
                | board = selectLocation model.step (Selection location (Answer Fail)) model.board
                , streak = 0
              }
            , Cmd.none
            )

        ( AnswerCorrect, Just { location } ) ->
            ( { model
                | board = selectLocation model.step (Selection location (Answer Pass)) model.board
                , step = model.step + 1
                , streak = model.streak + 1
              }
            , Cmd.none
            )

        ( NewTile tile, Just { location } ) ->
            ( { model
                | board = selectLocation model.step (Selection location tile) model.board
                , step = updateStep tile model.step
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


viewTileWithAction : Location -> Maybe Tile -> Html Msg
viewTileWithAction location tile =
    div
        [ class "tile tile-action"
        , class (tileToClass tile)
        , onClick (Select location)
        ]
        [ text (tileToString tile) ]


filterSelectionByLocation : Location -> Selection -> Maybe Selection
filterSelectionByLocation currentLocation selection =
    if currentLocation == selection.location then
        Just selection

    else
        Nothing


viewTileLocation : (Maybe Tile -> Html Msg) -> Location -> Maybe Selection -> Html Msg
viewTileLocation viewer currentLocation selection =
    selection
        |> Maybe.andThen (filterSelectionByLocation currentLocation)
        |> Maybe.map .tile
        |> viewer


selectionIsQuestion : Selection -> Bool
selectionIsQuestion { tile } =
    case tile of
        Question _ ->
            True

        _ ->
            False


selectionIsFail : Selection -> Bool
selectionIsFail { tile } =
    tile == Answer Fail


canSelectLocation : Maybe Selection -> Int -> Int -> Bool
canSelectLocation currentSelection currentStep step =
    currentSelection == Nothing && currentStep == step


viewLocationTiles : Bool -> Maybe Selection -> Html Msg
viewLocationTiles active selection =
    let
        viewer =
            if active then
                \location -> viewTileLocation (viewTileWithAction location) location selection

            else
                \location -> viewTileLocation viewTile location selection
    in
    [ Top, Middle, Bottom ]
        |> List.map viewer
        |> div [ classList [ ( "location", True ), ( "location-active", active ) ] ]


viewBoard : Player r -> Html Msg
viewBoard { playerName, step, board } =
    let
        currentSelection =
            getCurrentSelection step board

        currentCanSelectLocation =
            canSelectLocation currentSelection step
    in
    div [ class "active-player" ]
        [ h2 [ class "player-name" ] [ text playerName ]
        , board
            |> Array.indexedMap
                (\index selection ->
                    viewLocationTiles (currentCanSelectLocation index) selection
                )
            |> Array.toList
            |> div [ class "board" ]
        ]



-- Mini Board


viewLocation : Maybe Selection -> Html Msg
viewLocation selection =
    div [ class "location" ]
        [ selection |> Maybe.map .tile |> viewTile ]


viewMiniBoard : Player r -> Html Msg
viewMiniBoard { playerName, board } =
    div [ class "player mini-board" ]
        [ h2 [ class "player-name" ] [ text playerName ]
        , board
            |> Array.map viewLocation
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
        , a [ class "btn btn-pass", onClick AnswerCorrect ] [ text "Correct" ]
        , a [ class "btn btn-fail", onClick AnswerWrong ] [ text "Wrong" ]
        ]

    else if
        currentSelection
            |> Maybe.map selectionIsFail
            |> Maybe.withDefault False
    then
        [ span [ class "label" ] [ text "Game Over:" ]
        , a [ class "btn btn-try-again", onClick TryAgain ] [ text "Try Again" ]
        ]

    else if step < 9 then
        -- Choose Location
        [ span [ class "label" ] [ text "Choose:" ]
        , a [ class "btn btn-location btn-top", onClick (Select Top) ] [ text "Top" ]
        , a [ class "btn btn-location btn-middle", onClick (Select Middle) ] [ text "Middle" ]
        , a [ class "btn btn-location btn-bottom", onClick (Select Bottom) ] [ text "Bottom" ]
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
            ++ span [ class "spacer" ] []
            :: viewResetControl step currentSelection
        )


view : Player r -> Html Msg
view player =
    div [ class "main" ]
        [ h1 [] [ text "Strike It Lucky" ]
        , viewBoard player
        , viewControls player
        , viewMiniBoard player
        ]
