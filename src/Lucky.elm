module Lucky exposing (Model, Msg, init, update, view)

import Difficulty exposing (Difficulty)
import Html exposing (Html, a, div, h1, h2, span, text)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Random
import ZipList exposing (ZipList)



--- MODEL


type Question
    = Single
    | Team


type Answer
    = Pass
    | Fail


type Tile
    = Question Question
    | Answer Answer


type Location
    = Top
    | Middle
    | Bottom


type Selection
    = NotSelected
    | LoadingAt Location
    | QuestionAt Location Question
    | AnswerAt Location Answer


type alias Board =
    ZipList Selection


type alias Player r =
    { r
        | board : Board
        , playerName : String
    }


type alias Model =
    { difficulty : Difficulty
    , streak : Int
    , board : Board
    , playerName : String
    }


initialModel : Model
initialModel =
    { streak = 0
    , board = ZipList.fromLists [] NotSelected (List.repeat 8 NotSelected)
    , difficulty = Difficulty.fromInt 4
    , playerName = "Player 1"
    }


init : ( Model, Cmd Msg )
init =
    ( initialModel, Cmd.none )


getCurrentSelection : Board -> Selection
getCurrentSelection =
    ZipList.selected



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


clearLocation : Board -> Board
clearLocation =
    ZipList.update NotSelected


selectLocation : Selection -> Board -> Board
selectLocation selection =
    ZipList.update selection


updateSelection : Selection -> Model -> Model
updateSelection selection model =
    let
        newBoard =
            selectLocation selection model.board
    in
    case selection of
        AnswerAt _ Pass ->
            { model
                | board = newBoard |> ZipList.next
                , streak = model.streak + 1
            }

        AnswerAt _ Fail ->
            { model
                | board = newBoard
                , streak = 0
            }

        QuestionAt _ _ ->
            { model | board = newBoard }

        LoadingAt _ ->
            { model | board = newBoard }

        NotSelected ->
            model


updateNewTile : Tile -> Location -> Model -> Model
updateNewTile tile location model =
    case tile of
        Question question ->
            updateSelection (QuestionAt location question) model

        Answer answer ->
            updateSelection (AnswerAt location answer) model


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        currentSelection =
            getCurrentSelection model.board
    in
    case ( msg, currentSelection ) of
        ( Reset, _ ) ->
            init

        ( TryAgain, AnswerAt _ Fail ) ->
            ( { model | board = clearLocation model.board }
            , Cmd.none
            )

        ( Select location, NotSelected ) ->
            ( updateSelection (LoadingAt location) model
            , newTile model.difficulty model.streak
            )

        ( AnswerWrong, QuestionAt location _ ) ->
            ( updateSelection (AnswerAt location Fail) model
            , Cmd.none
            )

        ( AnswerCorrect, QuestionAt location _ ) ->
            ( updateSelection (AnswerAt location Pass) model
            , Cmd.none
            )

        ( NewTile tile, LoadingAt location ) ->
            ( updateNewTile tile location model
            , Cmd.none
            )

        -- Oops! Log this in dev?
        ( _, _ ) ->
            ( model, Cmd.none )



--- VIEW


selectionLocation : Selection -> Maybe Location
selectionLocation selection =
    case selection of
        NotSelected ->
            Nothing

        QuestionAt location _ ->
            Just location

        AnswerAt location _ ->
            Just location

        LoadingAt location ->
            Just location


viewTile : String -> String -> Html Msg
viewTile class_ label =
    div [ class "tile", class class_ ] [ text label ]


viewBlankTile : Html Msg
viewBlankTile =
    viewTile "tile-blank" "?"


viewActionTile : Location -> Html Msg
viewActionTile location =
    div
        [ class "tile tile-action tile-blank"
        , onClick (Select location)
        ]
        [ text "?" ]


viewSelection : Selection -> Html Msg
viewSelection selection =
    case selection of
        LoadingAt _ ->
            viewTile "tile-loading" "~"

        QuestionAt _ Single ->
            viewTile "tile-question" "Q"

        QuestionAt _ Team ->
            viewTile "tile-question" "QQ"

        AnswerAt _ Pass ->
            viewTile "tile-pass" ">>"

        AnswerAt _ Fail ->
            viewTile "tile-fail" "X"

        NotSelected ->
            viewBlankTile


viewSelectionAtLocation : Selection -> Location -> Html Msg
viewSelectionAtLocation selection location =
    if Just location == selectionLocation selection then
        viewSelection selection

    else
        viewSelection NotSelected


viewLocationTiles : ZipList.Position -> Selection -> Html Msg
viewLocationTiles position selection =
    div [ class "location" ]
        ([ Top, Middle, Bottom ]
            |> (if position == ZipList.Selected && selection == NotSelected then
                    List.map viewActionTile

                else
                    List.map (viewSelectionAtLocation selection)
               )
        )


viewBoard : Player r -> Html Msg
viewBoard { playerName, board } =
    div [ class "active-player" ]
        [ h2 [ class "player-name" ] [ text playerName ]
        , board
            |> ZipList.mapWithPosition viewLocationTiles
            |> ZipList.toList
            |> div [ class "board" ]
        ]



-- Mini Board


viewMiniBoard : Player r -> Html Msg
viewMiniBoard { playerName, board } =
    div [ class "player mini-board" ]
        [ h2 [ class "player-name" ] [ text playerName ]
        , board
            |> ZipList.map viewSelection
            |> ZipList.toList
            |> div [ class "board" ]
        ]


viewButton : Msg -> String -> String -> Html Msg
viewButton action class_ label =
    a [ class "btn", class class_, onClick action ] [ text label ]


viewQuestionControls : Selection -> List (Html Msg)
viewQuestionControls currentSelection =
    case currentSelection of
        QuestionAt _ _ ->
            [ span [ class "label" ] [ text "Answer:" ]
            , viewButton AnswerCorrect "btn-pass" "Correct"
            , viewButton AnswerWrong "btn-fail" "Wrong"
            ]

        AnswerAt _ Fail ->
            [ span [ class "label" ] [ text "Game Over:" ]
            , viewButton TryAgain "btn-try-again" "Try Again"
            ]

        NotSelected ->
            [ span [ class "label" ] [ text "Choose:" ]
            , viewButton (Select Top) "btn-location btn-top" "Top"
            , viewButton (Select Middle) "btn-location btn-middle" "Middle"
            , viewButton (Select Bottom) "btn-location btn-bottom" "Bottom"
            ]

        -- Game Complete or Selection Loading
        _ ->
            []


viewResetControl : Board -> List (Html Msg)
viewResetControl board =
    if ZipList.before board == [] && ZipList.selected board == NotSelected then
        [ a [ class "btn btn-disabled" ] [ text "Reset" ] ]

    else
        [ viewButton Reset "btn-reset" "Reset" ]


viewControls : Player r -> Html Msg
viewControls { board } =
    let
        currentSelection =
            getCurrentSelection board
    in
    div [ class "controls" ]
        (viewQuestionControls currentSelection
            ++ span [ class "spacer" ] []
            :: viewResetControl board
        )


view : Player r -> Html Msg
view player =
    div [ class "main" ]
        [ h1 [] [ text "Strike It Lucky" ]
        , viewBoard player
        , viewControls player
        , viewMiniBoard player
        ]
