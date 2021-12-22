module Home exposing (Model, Msg, init, update, view)

import Browser
import Html exposing (..)

main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }

type alias Model = {}

initialModel : Model
initialModel = {}

init : () -> ( Model, Cmd Msg )
init () = ( initialModel, Cmd.none )

type alias Msg = {}

update : Msg -> Model -> (Model, Cmd Msg)
update _ m = ( m, Cmd.none )

view : Model -> Html Msg
view _ =
    div [] [ text "Welcome to the EbMS Admin Console" ]