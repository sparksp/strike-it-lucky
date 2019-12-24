module Lucky exposing (Model, Msg, init, update, view)

import Board exposing (Board)
import Difficulty exposing (Difficulty)
import Html exposing (Html, a, div, h1, h2, h3, span, text)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Lucky.Settings exposing (Settings)
import Random
import ZipList exposing (ZipList)



--- MODEL


type RandomSelection
    = RandomQuestion Board.Question
    | RandomAnswer Board.Answer


type alias Player =
    { board : Board
    , playerName : String
    }


type alias Model =
    { difficulty : Difficulty
    , streak : Int
    , players : ZipList Player
    }


initPlayer : String -> Player
initPlayer =
    Player Board.new


initialModel : Settings -> Model
initialModel { difficulty, playerName, morePlayerNames } =
    { difficulty = difficulty
    , streak = 0
    , players =
        ZipList.fromLists []
            (initPlayer playerName)
            (List.map initPlayer morePlayerNames)
    }


init : Settings -> ( Model, Cmd Msg )
init settings =
    ( initialModel settings, Cmd.none )


resetPlayer : Player -> Player
resetPlayer player =
    { player | board = Board.new }


reset : Model -> Model
reset model =
    { model
        | streak = 0
        , players = model.players |> ZipList.map resetPlayer |> ZipList.rewind
    }



--- COMMANDS


randomSelection : Difficulty -> Int -> Random.Generator RandomSelection
randomSelection difficulty streak =
    Random.weighted
        ( 15, RandomQuestion Board.Single )
        [ ( 4, RandomQuestion Board.Team )
        , ( toFloat ((Difficulty.toInt difficulty * streak) + 2), RandomAnswer Board.Fail )
        , ( 4, RandomAnswer Board.Pass )
        ]


newTile : Difficulty -> Int -> Cmd Msg
newTile difficulty streak =
    Random.generate NewRandomSelection (randomSelection difficulty streak)



--- UPDATE


type Msg
    = Select Board.Location
    | NewRandomSelection RandomSelection
    | Answer Board.Answer
    | FinalAnswer Board.Answer
    | Reset


updatePlayerBoard : (Board -> Board) -> ZipList Player -> ZipList Player
updatePlayerBoard fn players =
    ZipList.mapSelected
        (\player -> { player | board = fn player.board })
        players


updateBoard : (Board -> Board) -> Model -> Model
updateBoard fn model =
    { model | players = updatePlayerBoard fn model.players }


updateAnswer : Board.Answer -> Model -> Model
updateAnswer answer model =
    { model | players = updatePlayerBoard (Board.answer answer) model.players }
        |> updateStreak answer
        |> switchPlayer answer


updateFinalAnswer : Board.Answer -> Model -> Model
updateFinalAnswer answer model =
    { model | players = updatePlayerBoard (Board.finalAnswer answer) model.players }
        |> switchPlayer answer


updateStreak : Board.Answer -> Model -> Model
updateStreak answer model =
    case answer of
        Board.Fail ->
            { model | streak = 0 }

        Board.Pass ->
            { model | streak = 1 + model.streak }


switchPlayer : Board.Answer -> Model -> Model
switchPlayer answer model =
    case answer of
        Board.Fail ->
            { model | players = ZipList.loop model.players |> updatePlayerBoard Board.clearFinalAnswer }

        Board.Pass ->
            model


updateRandomSelection : RandomSelection -> Model -> Model
updateRandomSelection selection model =
    case selection of
        RandomQuestion question ->
            updateBoard (Board.question question) model

        RandomAnswer answer ->
            updateAnswer answer model


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Reset ->
            ( reset model, Cmd.none )

        Select location ->
            ( updateBoard (Board.loading location) model
            , newTile model.difficulty model.streak
            )

        Answer answer ->
            ( updateAnswer answer model
            , Cmd.none
            )

        FinalAnswer answer ->
            ( updateFinalAnswer answer model
            , Cmd.none
            )

        NewRandomSelection selection ->
            ( updateRandomSelection selection model
            , Cmd.none
            )



--- VIEW


selectionLocation : Board.Selection -> Maybe Board.Location
selectionLocation selection =
    case selection of
        Board.NotSelected ->
            Nothing

        Board.QuestionAt location _ ->
            Just location

        Board.LoadingAt location ->
            Just location


viewTile : String -> String -> Html Msg
viewTile class_ label =
    div [ class "tile", class class_ ] [ text label ]


viewBlankTile : Html Msg
viewBlankTile =
    viewTile "tile-blank" "?"


viewActionTile : Board.Location -> Html Msg
viewActionTile location =
    div
        [ class "tile tile-action tile-blank"
        , onClick (Select location)
        ]
        [ text "?" ]


viewSelection : Board.Selection -> Html Msg
viewSelection selection =
    case selection of
        Board.LoadingAt _ ->
            viewTile "tile-loading" "~"

        Board.QuestionAt _ Board.Single ->
            viewTile "tile-question" "Q"

        Board.QuestionAt _ Board.Team ->
            viewTile "tile-question" "QQ"

        Board.NotSelected ->
            viewBlankTile


viewSelectionAtLocation : Board.Selection -> Board.Location -> Html Msg
viewSelectionAtLocation selection location =
    if Just location == selectionLocation selection then
        viewSelection selection

    else
        viewSelection Board.NotSelected


viewCurrentLocation : Board.Selection -> Html Msg
viewCurrentLocation selection =
    div [ class "location" ]
        ([ Board.Top, Board.Middle, Board.Bottom ]
            |> (if selection == Board.NotSelected then
                    List.map viewActionTile

                else
                    List.map (viewSelectionAtLocation selection)
               )
        )


viewAnswer : Board.Answer -> Html Msg
viewAnswer answer =
    case answer of
        Board.Pass ->
            viewTile "tile-answer tile-pass" "→"

        Board.Fail ->
            viewTile "tile-answer tile-fail" "X"


viewAnswered : Board.Answered -> Html Msg
viewAnswered (Board.AnswerAt _ answer) =
    viewAnswer answer


viewAnsweredAt : Board.Answered -> Board.Location -> Html Msg
viewAnsweredAt (Board.AnswerAt answerLocation answer) location =
    if answerLocation == location then
        viewAnswer answer

    else
        viewBlankTile


viewLocationAnswered : Board.Answered -> Html Msg
viewLocationAnswered answered =
    div [ class "location" ]
        (List.map (viewAnsweredAt answered) [ Board.Top, Board.Middle, Board.Bottom ])


viewBlankLocation : Html Msg
viewBlankLocation =
    div [ class "location" ]
        (List.repeat 3 viewBlankTile)


viewFinal : Board.FinalSelection -> Html Msg
viewFinal selection =
    case selection of
        Board.FinalFuture ->
            viewBlankTile

        Board.FinalQuestion ->
            viewTile "tile-question" "QQ"

        Board.FinalAnswer Board.Pass ->
            viewTile "tile-answer tile-pass" "✓"

        Board.FinalAnswer answer ->
            viewAnswer answer


viewLocation : Board.Tile -> Html Msg
viewLocation tile =
    case tile of
        Board.Answer answer ->
            viewLocationAnswered answer

        Board.Selection selection ->
            viewCurrentLocation selection

        Board.Future ->
            viewBlankLocation

        Board.Final final ->
            div [ class "location" ]
                [ div [ class "tile tile-spacer" ] []
                , viewFinal final
                , div [ class "tile tile-spacer" ] []
                ]


viewBoard : Player -> Html Msg
viewBoard { playerName, board } =
    div [ class "active-player" ]
        [ h2 [ class "player-name" ] [ text playerName ]
        , div [ class "board" ]
            (Board.map viewLocation board)
        ]



-- Mini Board


viewMiniLocation : Board.Tile -> Html Msg
viewMiniLocation tile =
    case tile of
        Board.Answer answer ->
            viewAnswered answer

        Board.Selection selection ->
            viewSelection selection

        Board.Future ->
            viewBlankTile

        Board.Final final ->
            viewFinal final


viewMiniBoards : ZipList Player -> Html Msg
viewMiniBoards players =
    players
        |> (ZipList.mapWithPosition <|
                \position player ->
                    if position /= ZipList.Selected then
                        viewMiniBoard player

                    else
                        div [] []
           )
        |> ZipList.toList
        |> div [ class "other-players" ]


viewMiniBoard : Player -> Html Msg
viewMiniBoard { playerName, board } =
    div [ class "player mini-board" ]
        [ h3 [ class "player-name" ] [ text playerName ]
        , div [ class "board" ]
            (List.map
                (List.singleton >> div [ class "location" ])
                (Board.map viewMiniLocation board)
            )
        ]


viewButton : Msg -> String -> String -> Html Msg
viewButton action class_ label =
    a [ class "btn", class class_, onClick action ] [ text label ]


viewQuestionControls : Board.Tile -> List (Html Msg)
viewQuestionControls tile =
    case tile of
        Board.Final Board.FinalQuestion ->
            [ span [ class "label" ] [ text "Final Question:" ]
            , viewButton (FinalAnswer Board.Pass) "btn-pass" "Correct"
            , viewButton (FinalAnswer Board.Fail) "btn-fail" "Wrong"
            ]

        Board.Final (Board.FinalAnswer Board.Pass) ->
            [ span [ class "label" ] [ text "Congratulations!" ] ]

        Board.Selection (Board.QuestionAt _ _) ->
            [ span [ class "label" ] [ text "Answer:" ]
            , viewButton (Answer Board.Pass) "btn-pass" "Correct"
            , viewButton (Answer Board.Fail) "btn-fail" "Wrong"
            ]

        Board.Selection Board.NotSelected ->
            [ span [ class "label" ] [ text "Choose:" ]
            , viewButton (Select Board.Top) "btn-location btn-top" "Top"
            , viewButton (Select Board.Middle) "btn-location btn-middle" "Middle"
            , viewButton (Select Board.Bottom) "btn-location btn-Bottom" "Bottom"
            ]

        Board.Selection (Board.LoadingAt _) ->
            [ span [ class "label" ] [ text "Picking a question..." ] ]

        -- Oops?
        _ ->
            []


viewResetControl : List (Html Msg)
viewResetControl =
    [ span [ class "spacer" ] []
    , viewButton Reset "btn-reset" "Reset"
    ]


viewControls : Player -> Html Msg
viewControls { board } =
    div [ class "controls" ]
        (viewQuestionControls (Board.selected board)
            ++ viewResetControl
        )


view : Model -> Html Msg
view model =
    let
        player =
            ZipList.selected model.players
    in
    div [ class "main" ]
        [ h1 [] [ text "Strike It Lucky" ]
        , viewBoard player
        , viewControls player
        , viewMiniBoards model.players
        ]
