module Lucky exposing (Model, Msg, initialModel, update, view)

import Array exposing (Array)
import Html exposing (Html, a, div, h1, span, text)
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
    ( Space, Tile )


type alias Board =
    Array (Maybe Selection)


type alias Model =
    { step : Int
    , streak : Int
    , space : Maybe Space
    , board : Board
    }


initialModel : Model
initialModel =
    { step = 0
    , streak = 0
    , space = Nothing
    , board = Array.repeat 9 Nothing
    }


getCurrentSelection : Model -> Maybe Selection
getCurrentSelection model =
    Array.get model.step model.board
        |> Maybe.withDefault Nothing



--- COMMANDS


randomTile : Int -> Random.Generator Tile
randomTile streak =
    Random.weighted
        ( 15, Question Single )
        [ ( 4, Question Team )
        , ( toFloat (4 * streak + 2), HotSpot )
        , ( 4, Answer Pass )
        ]


newTile : Int -> Cmd Msg
newTile streak =
    Random.generate NewTile (randomTile streak)



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
            ( initialModel, Cmd.none )

        ( Select space, Nothing ) ->
            ( { model
                | board = selectSpace model.step ( space, Loading ) model.board
                , space = Just space
              }
            , newTile model.streak
            )

        ( NewAnswer answer, Just space ) ->
            let
                tile =
                    Answer answer
            in
            ( { model
                | board = selectSpace model.step ( space, tile ) model.board
                , step = updateStep tile model.step
                , space = updateSpace tile space
                , streak = updateStreak tile model.streak
              }
            , Cmd.none
            )

        ( NewTile tile, Just space ) ->
            ( { model
                | board = selectSpace model.step ( space, tile ) model.board
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


viewTileSpace : (Maybe Tile -> Html Msg) -> Space -> Maybe Selection -> Html Msg
viewTileSpace viewer currentSpace selection =
    case selection of
        Nothing ->
            viewer Nothing

        Just ( space, tile ) ->
            if currentSpace == space then
                viewer (Just tile)

            else
                viewer Nothing


canSelectSpace : Model -> Int -> Bool
canSelectSpace model step =
    case getCurrentSelection model of
        Just ( _, Question _ ) ->
            False

        _ ->
            if model.step < 9 && model.step == step then
                True

            else
                False


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


viewBoard : Model -> Html Msg
viewBoard model =
    div [ class "board" ]
        (model.board
            |> Array.indexedMap
                (\index selection ->
                    viewSpaceTiles (canSelectSpace model index) selection
                )
            |> Array.toList
        )



-- Mini Board


viewSpace : Maybe Selection -> Html Msg
viewSpace selection =
    div [ class "space" ]
        (case selection of
            Just ( _, tile ) ->
                [ viewTile (Just tile) ]

            Nothing ->
                [ viewTile Nothing ]
        )


viewMiniBoard : Model -> Html Msg
viewMiniBoard model =
    div [ class "board mini-board" ]
        (model.board
            |> Array.map viewSpace
            |> Array.toList
        )


viewQuestionControls : Model -> List (Html Msg)
viewQuestionControls model =
    case getCurrentSelection model of
        Just ( _, Question _ ) ->
            [ span [ class "label" ] [ text "Answer:" ]
            , a [ class "btn btn-pass", onClick (NewAnswer Pass) ] [ text "Correct" ]
            , a [ class "btn btn-fail", onClick (NewAnswer Fail) ] [ text "Wrong" ]
            ]

        _ ->
            if model.step < 9 then
                [ span [ class "label" ] [ text "Choose:" ]
                , a [ class "btn btn-space btn-top", onClick (Select Top) ] [ text "Top" ]
                , a [ class "btn btn-space btn-middle", onClick (Select Middle) ] [ text "Middle" ]
                , a [ class "btn btn-space btn-bottom", onClick (Select Bottom) ] [ text "Bottom" ]
                ]

            else
                []


viewResetControl : Model -> List (Html Msg)
viewResetControl model =
    if model.step == 0 && getCurrentSelection model == Nothing then
        [ a [ class "btn btn-disabled" ] [ text "Reset" ] ]

    else
        [ a [ class "btn btn-reset", onClick Reset ] [ text "Reset" ] ]


viewControls : Model -> Html Msg
viewControls model =
    div [ class "controls" ]
        (viewQuestionControls model
            ++ [ span [ class "spacer" ] [] ]
            ++ viewResetControl model
        )


view : Model -> Html Msg
view model =
    div [ class "main" ]
        [ h1 [] [ text "Strike It Lucky" ]
        , viewBoard model
        , viewControls model
        , viewMiniBoard model
        ]
