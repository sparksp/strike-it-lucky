module Lucky.Setup exposing
    ( Model, InternalMsg, init, update, view
    , Translator, translator
    )

{-| A form to generate `Lucky.Settings`

@docs Model, InternalMsg, init, update, view
@docs Translator, translator

-}

import Array exposing (Array)
import Browser.Dom as Dom
import Difficulty exposing (Difficulty)
import Html exposing (Html, div, h1, h2, input, label, span, text)
import Html.Attributes as Attr exposing (class, type_, value)
import Html.Events exposing (onClick, onInput, onSubmit)
import Lucky.Settings exposing (Settings)
import Task


type alias Model =
    { difficulty : Difficulty
    , playerName : String
    , playerNames : Array String
    }


initialModel : Model
initialModel =
    { difficulty = Difficulty.fromInt 4
    , playerName = "Player 1"
    , playerNames = Array.fromList [ "Player 2" ]
    }


init : ( Model, Cmd Msg )
init =
    ( initialModel, Cmd.none )


type InternalMsg
    = SetDifficulty String
    | AddPlayer
    | SetPlayerName String
    | SetPlayerNameAt Int String
    | RemovePlayerAt Int
    | Focus (Result Dom.Error ())


type ExternalMsg
    = StartGame Settings


type Msg
    = ForSelf InternalMsg
    | ForParent ExternalMsg


type alias Translator parentMsg =
    Msg -> parentMsg


type alias TranslationDictionary msg =
    { onInternalMsg : InternalMsg -> msg
    , onStartGame : Settings -> msg
    }


translator : TranslationDictionary parentMsg -> Translator parentMsg
translator { onInternalMsg, onStartGame } msg =
    case msg of
        ForSelf internalMsg ->
            onInternalMsg internalMsg

        ForParent (StartGame settings_) ->
            onStartGame settings_


arrayRemove : Int -> Array a -> Array a
arrayRemove index array =
    Array.append
        (Array.slice 0 index array)
        (Array.slice (index + 1) (Array.length array) array)


settings : Model -> Settings
settings model =
    { difficulty = model.difficulty
    , playerName = model.playerName
    , morePlayerNames = Array.toList model.playerNames
    }


update : InternalMsg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetDifficulty n ->
            ( { model
                | difficulty = String.toInt n |> Maybe.map Difficulty.fromInt |> Maybe.withDefault model.difficulty
              }
            , Cmd.none
            )

        AddPlayer ->
            let
                numberOfPlayers =
                    Array.length model.playerNames + 2

                newPlayerName =
                    "Player " ++ String.fromInt numberOfPlayers
            in
            ( { model
                | playerNames = Array.push newPlayerName model.playerNames
              }
            , Dom.focus (playerDomId (Array.length model.playerNames)) |> Task.attempt (Focus >> ForSelf)
            )

        Focus _ ->
            ( model, Cmd.none )

        SetPlayerName newName ->
            ( { model
                | playerName = newName
              }
            , Cmd.none
            )

        SetPlayerNameAt index newName ->
            ( { model
                | playerNames = Array.set index newName model.playerNames
              }
            , Cmd.none
            )

        RemovePlayerAt index ->
            ( { model
                | playerNames = arrayRemove index model.playerNames
              }
            , Cmd.none
            )


playerDomId : Int -> String
playerDomId index =
    "player-name-" ++ String.fromInt (index + 1)


inputPlayerName : (String -> Msg) -> String -> String -> Html Msg
inputPlayerName msg id playerName =
    input
        [ type_ "text"
        , class "form-input form-input-text"
        , value playerName
        , onInput msg
        , Attr.id id
        , Attr.placeholder "Player Name"
        , Attr.required True
        ]
        []


inputDifficulty : (String -> Msg) -> Difficulty -> List (Html Msg)
inputDifficulty msg value =
    [ span [ class "form-range-label" ] [ text (Difficulty.min |> Difficulty.toString) ]
    , input
        [ type_ "range"
        , class "form-input"
        , Attr.min (Difficulty.min |> Difficulty.toString)
        , Attr.max (Difficulty.max |> Difficulty.toString)
        , Attr.value (value |> Difficulty.toString)
        , onInput msg
        , Attr.title (value |> Difficulty.toString)
        ]
        []
    , span [ class "form-range-label" ] [ text (Difficulty.max |> Difficulty.toString) ]
    ]


inputButton : String -> Msg -> Html Msg
inputButton label msg =
    input
        [ type_ "button"
        , class "form-btn btn"
        , value label
        , onClick msg
        ]
        []


view : Model -> Html Msg
view model =
    div [ class "setup" ]
        [ h1 [] [ text "Strike It Lucky" ]
        , Html.form [ onSubmit (settings model |> StartGame |> ForParent) ]
            [ h2 [] [ text "Settings" ]
            , div [ class "input-group difficulty" ]
                [ label [ class "form-label" ] [ text "Difficulty" ]
                , div [ class "form-input-group" ] (inputDifficulty (SetDifficulty >> ForSelf) model.difficulty)
                ]
            , div [ class "form-label" ] [ text "Players" ]
            , Html.ol [ class "form-list player-name" ]
                (Html.li []
                    [ div [ class "form-input-group" ]
                        [ inputPlayerName (SetPlayerName >> ForSelf) "player-name-0" model.playerName ]
                    ]
                    :: List.indexedMap
                        (\index name ->
                            Html.li []
                                [ div [ class "form-input-group" ]
                                    [ inputPlayerName (SetPlayerNameAt index >> ForSelf) (playerDomId index) name
                                    , inputButton "X" (RemovePlayerAt index |> ForSelf)
                                    ]
                                ]
                        )
                        (Array.toList model.playerNames)
                )
            , div [ class "input-group form-controls" ]
                [ inputButton "Add Player" (AddPlayer |> ForSelf)
                , Html.span [ class "spacer" ] []
                , input [ type_ "submit", class "form-btn btn", value "Start Game" ] []
                ]
            ]
        ]
