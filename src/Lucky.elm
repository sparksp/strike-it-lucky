module Lucky exposing (Model, Msg, init, update, view)

import Difficulty exposing (Difficulty)
import Html exposing (Html, a, div, h1, h2, span, text)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Lucky.Settings exposing (Settings)
import Random
import ZipList exposing (ZipList)



--- MODEL


type Question
    = Single
    | Team


type Answer
    = Pass
    | Fail


type RandomSelection
    = RandomQuestion Question
    | RandomAnswer Answer


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


type alias Player =
    { board : Board
    , playerName : String
    }


type alias Model =
    { difficulty : Difficulty
    , streak : Int
    , players : ZipList Player
    }


initBoard : Board
initBoard =
    ZipList.fromLists [] NotSelected (List.repeat 8 NotSelected)


initPlayer : String -> Player
initPlayer =
    Player initBoard


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
    { player | board = initBoard }


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
        ( 15, RandomQuestion Single )
        [ ( 4, RandomQuestion Team )
        , ( toFloat ((Difficulty.toInt difficulty * streak) + 2), RandomAnswer Fail )
        , ( 4, RandomAnswer Pass )
        ]


newTile : Difficulty -> Int -> Cmd Msg
newTile difficulty streak =
    Random.generate NewRandomSelection (randomSelection difficulty streak)



--- UPDATE


type Msg
    = Select Location
    | NewRandomSelection RandomSelection
    | Answer Answer
    | NextPlayer
    | Reset


updatePlayerBoard : (Board -> Board) -> ZipList Player -> ZipList Player
updatePlayerBoard fn players =
    ZipList.mapSelected
        (\player -> { player | board = fn player.board })
        players


updateSelection : Selection -> Model -> Model
updateSelection selection model =
    let
        newModel =
            { model | players = updatePlayerBoard (ZipList.update selection) model.players }
    in
    case selection of
        AnswerAt _ Pass ->
            { newModel
                | players = updatePlayerBoard ZipList.next newModel.players
                , streak = model.streak + 1
            }

        AnswerAt _ Fail ->
            { newModel
                | streak = 0
            }

        QuestionAt _ _ ->
            newModel

        LoadingAt _ ->
            newModel

        NotSelected ->
            newModel


updateRandomSelection : RandomSelection -> Location -> Model -> Model
updateRandomSelection selection location model =
    case selection of
        RandomQuestion question ->
            updateSelection (QuestionAt location question) model

        RandomAnswer answer ->
            updateSelection (AnswerAt location answer) model


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        currentSelection =
            ZipList.selected model.players
                |> .board
                |> ZipList.selected
    in
    case ( msg, currentSelection ) of
        ( Reset, _ ) ->
            ( reset model, Cmd.none )

        ( NextPlayer, AnswerAt _ Fail ) ->
            ( { model
                | players =
                    model.players
                        |> updatePlayerBoard ZipList.next
                        |> ZipList.loop
              }
            , Cmd.none
            )

        ( Select location, NotSelected ) ->
            ( updateSelection (LoadingAt location) model
            , newTile model.difficulty model.streak
            )

        ( Answer answer, QuestionAt location _ ) ->
            ( updateSelection (AnswerAt location answer) model
            , Cmd.none
            )

        ( NewRandomSelection selection, LoadingAt location ) ->
            ( updateRandomSelection selection location model
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


viewBoard : Player -> Html Msg
viewBoard { playerName, board } =
    div [ class "active-player" ]
        [ h2 [ class "player-name" ] [ text playerName ]
        , board
            |> ZipList.mapWithPosition viewLocationTiles
            |> ZipList.toList
            |> div [ class "board" ]
        ]



-- Mini Board


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
            , viewButton (Answer Pass) "btn-pass" "Correct"
            , viewButton (Answer Fail) "btn-fail" "Wrong"
            ]

        AnswerAt _ Fail ->
            [ span [ class "label" ] [ text "Oops:" ]
            , viewButton NextPlayer "btn-next-player" "Next Player"
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


viewResetControl : List (Html Msg)
viewResetControl =
    [ span [ class "spacer" ] []
    , viewButton Reset "btn-reset" "Reset"
    ]


viewControls : Player -> Html Msg
viewControls { board } =
    let
        currentSelection =
            ZipList.selected board
    in
    div [ class "controls" ]
        (viewQuestionControls currentSelection
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
