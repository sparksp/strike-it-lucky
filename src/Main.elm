module Main exposing (main)

import Browser exposing (Document)
import Html
import Lucky


type Page
    = Lucky Lucky.Model


type alias Model =
    { page : Page }


initialModel : Model
initialModel =
    { page = Lucky Lucky.initialModel
    }


init : () -> ( Model, Cmd Msg )
init () =
    ( initialModel, Cmd.none )


type Msg
    = LuckyMsg Lucky.Msg


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
        ( LuckyMsg luckyMsg, Lucky luckyModel ) ->
            Lucky.update luckyMsg luckyModel
                |> processPageUpdate Lucky LuckyMsg model


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


view : Model -> Document Msg
view model =
    case model.page of
        Lucky luckyModel ->
            { title = "Strike It Lucky"
            , body = [ Html.map LuckyMsg (Lucky.view luckyModel) ]
            }



-- MAIN


main : Program () Model Msg
main =
    Browser.document
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }
