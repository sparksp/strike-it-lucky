module Main exposing (Model, Msg, Page, main)

import Browser exposing (Document)
import Html
import Lucky
import Lucky.Settings exposing (Settings)
import Lucky.Setup


type Page
    = Setup Lucky.Setup.Model
    | Lucky Lucky.Model


type alias Model =
    { page : Page }


init : () -> ( Model, Cmd Msg )
init () =
    let
        ( setupModel, cmd ) =
            Lucky.Setup.init
    in
    ( { page = Setup setupModel }, Cmd.map setupTranslator cmd )


type Msg
    = SetupMsg Lucky.Setup.InternalMsg
    | LuckyMsg Lucky.Msg
    | PlayGame Settings


setupTranslator : Lucky.Setup.Translator Msg
setupTranslator =
    Lucky.Setup.translator
        { onInternalMsg = SetupMsg
        , onStartGame = PlayGame
        }


processPageUpdate :
    (pageModel -> Page)
    -> (pageMsg -> Msg)
    -> Model
    -> ( pageModel, Cmd pageMsg )
    -> ( Model, Cmd Msg )
processPageUpdate createPage wrapMsg model ( pageModel, pageCmd ) =
    ( { model | page = createPage pageModel }
    , Cmd.map wrapMsg pageCmd
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model.page ) of
        ( SetupMsg setupMsg, Setup setupModel ) ->
            Lucky.Setup.update setupMsg setupModel
                |> processPageUpdate Setup setupTranslator model

        ( LuckyMsg luckyMsg, Lucky luckyModel ) ->
            Lucky.update luckyMsg luckyModel
                |> processPageUpdate Lucky LuckyMsg model

        ( PlayGame settings, _ ) ->
            Lucky.init settings
                |> processPageUpdate Lucky LuckyMsg model

        -- Oops
        _ ->
            ( model, Cmd.none )


view : Model -> Document Msg
view model =
    case model.page of
        Setup setupModel ->
            { title = "Setup: Strike It Lucky"
            , body = [ Lucky.Setup.view setupModel |> Html.map setupTranslator ]
            }

        Lucky luckyModel ->
            { title = "Strike It Lucky"
            , body = [ Lucky.view luckyModel |> Html.map LuckyMsg ]
            }



-- MAIN


main : Program () Model Msg
main =
    Browser.document
        { init = init
        , update = update
        , view = view
        , subscriptions = \_ -> Sub.none
        }
