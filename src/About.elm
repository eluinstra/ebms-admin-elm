module About exposing (Model, Msg, init, subscriptions, update, view)

import Bootstrap.Accordion as Accordion
import Bootstrap.Card.Block as Block
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


type alias Model =
    { accordionState : Accordion.State }


initialModel : Model
initialModel =
    { accordionState = Accordion.initialStateCardOpen "version" }


init : () -> ( Model, Cmd Msg )
init () =
    ( initialModel, Cmd.none )


type Msg
    = AccordionMsg Accordion.State


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        AccordionMsg state ->
            ( { model | accordionState = state }, Cmd.none )


view : Model -> Html Msg
view model =
    div []
        [ h1 [] [ text "About" ]
        , Accordion.config AccordionMsg
            |> Accordion.withAnimation
            |> Accordion.cards
                [ Accordion.card
                    { id = "version"
                    , options = []
                    , header =
                        Accordion.header [] <| Accordion.toggle [] [ text "Version" ]
                    , blocks =
                        [ Accordion.block []
                            [ Block.text [] [ text "EbMS Admin Console: 2.18.x" ]
                            , Block.text [] [ text "EbMS Core: 2.18.x" ]
                            ]
                        ]
                    }
                , Accordion.card
                    { id = "license"
                    , options = []
                    , header =
                        Accordion.header [] <| Accordion.toggle [] [ text "License" ]
                    , blocks =
                        [ Accordion.block []
                            [ Block.text [] [ pre [] [ text """Copyright 2021 E.Luinstra
Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at
  http://www.apache.org/licenses/LICENSE-2.0
Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.""" ] ] ]
                        ]
                    }
                ]
            |> Accordion.view model.accordionState
        ]


subscriptions : Model -> Sub Msg
subscriptions model =
    Accordion.subscriptions model.accordionState AccordionMsg
