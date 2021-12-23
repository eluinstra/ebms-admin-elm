module Ping exposing (Model, Msg, init, update, view)

import CpaDecoder exposing (getPartyIds)
import Bootstrap.Button as Button
import Bootstrap.Form as Form
import Bootstrap.Form.Select as Select
import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Http
import Json.Decode as JSonDecode

main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = \_ -> Sub.none
        }


type alias CpaId =
    String


type CpaIds
    = Loading
    | Success (List CpaId)
    | Failure String


type Cpa
    = CpaNothing
    | CpaLoading
    | CpaSuccess String
    | CpaFailure String


type alias Model =
    { cpaIds : CpaIds
    , selectedCpaId : Maybe CpaId
    , cpa : Cpa
    , fromPartyIds : List String
    , selectedFromPartyId : Maybe String
    , toPartyIds : List String
    , selectedToParty : Maybe String
    }


initialModel : Model
initialModel =
    { cpaIds = Loading
    , selectedCpaId = Nothing
    , cpa = CpaNothing
    , fromPartyIds = []
    , selectedFromPartyId = Nothing
    , toPartyIds = []
    , selectedToParty = Nothing
    }


initialCmd : Cmd Msg
initialCmd =
    getCpaIds ()


init : () -> ( Model, Cmd Msg )
init () =
    ( initialModel
    , initialCmd
    )


type Msg
    = ReceivedCpaIds (Result Http.Error (List String))
    | SelectCpaId CpaId
    | ReceivedCpa (Result Http.Error String)
    | SelectFromParty String
    | SelectToParty String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ReceivedCpaIds (Ok cpaIds) ->
            ( { model | cpaIds = Success cpaIds }, Cmd.none )

        ReceivedCpaIds (Err httpError) ->
            ( { model | cpaIds = Failure (buildErrorMessage httpError) }, Cmd.none )

        SelectCpaId cpaId ->
            ( { model | selectedCpaId = Just cpaId, cpa = CpaLoading }, getCpa cpaId )

        ReceivedCpa (Ok cpa) ->
            ( { model | cpa = CpaSuccess cpa, fromPartyIds = getPartyIds cpa }, Cmd.none )

        ReceivedCpa (Err httpError) ->
            ( { model | cpa = CpaFailure (buildErrorMessage httpError) }, Cmd.none )

        SelectFromParty partyId ->
            ( { model | selectedFromPartyId = Just partyId, toPartyIds = 
                (case model.cpa of
                    CpaSuccess cpa -> getPartyIds cpa
                    _-> [])
                |> List.filter (\id -> id == partyId) }, Cmd.none )

        SelectToParty partyId ->
            ( { model | selectedToParty = Just partyId }, Cmd.none )

getCpaIds : () -> Cmd Msg
getCpaIds () =
    Http.get
        { url = "http://localhost:8080/service/rest/v18/cpas"
        , expect = Http.expectJson ReceivedCpaIds cpaIdsDecoder
        }


cpaIdsDecoder : JSonDecode.Decoder (List String)
cpaIdsDecoder =
    JSonDecode.list JSonDecode.string


getCpa : String -> Cmd Msg
getCpa cpaId =
    Http.get
        { url = "http://localhost:8080/service/rest/v18/cpas/" ++ cpaId
        , expect = Http.expectString ReceivedCpa
        }

buildErrorMessage : Http.Error -> String
buildErrorMessage httpError =
    case httpError of
        Http.BadUrl message ->
            message

        Http.Timeout ->
            "Server is taking too long to respond. Please try again later."

        Http.NetworkError ->
            "Unable to reach server."

        Http.BadStatus statusCode ->
            "Request failed with status code: " ++ String.fromInt statusCode

        Http.BadBody message ->
            message


selectItems : List String -> List (Select.Item msg)
selectItems l =
    List.map (\s -> Select.item [ value s ] [ text s ]) l


optionalSelectList : List (Select.Item msg) -> Maybe a -> List (Select.Item msg)
optionalSelectList l x =
    if List.length l > 0 && x == Nothing then
        Select.item [] [ text "Select one" ] :: l

    else
        l


view : Model -> Html Msg
view model =
    case model.cpaIds of
        Loading ->
            text "Loading"

        Success cpaIds ->
            viewForm model cpaIds

        Failure msg ->
            text msg


viewForm : Model -> List CpaId -> Html Msg
viewForm model cpaIds =
    Form.form []
        [ Form.group []
            [ Form.label [ for "cpaId" ] [ text "CPA" ]
            , Select.select [ Select.id "cpaId", Select.onChange SelectCpaId ] <|
                optionalSelectList (selectItems cpaIds) model.selectedCpaId
            ]
        , Form.group []
            [ Form.label [ for "fromPartyId" ] [ text "From Party" ]
            , Select.select [ Select.id "fromPartyId", Select.disabled (List.isEmpty model.fromPartyIds), Select.onChange SelectFromParty ] <|
                optionalSelectList (selectItems model.fromPartyIds) model.selectedFromPartyId
            ]
        , Form.group []
            [ Form.label [ for "toPartyId" ] [ text "To Party" ]
            , Select.select [ Select.id "toPartyId", Select.disabled (List.isEmpty model.toPartyIds), Select.onChange SelectToParty ] <|
                optionalSelectList (selectItems model.toPartyIds) model.selectedToParty
            ]
        , Button.button [ Button.primary, Button.disabled (model.selectedToParty == Nothing) ] [ text "Ping" ]
        , div []
            [ case model.cpa of
                CpaSuccess cpa ->
                    text cpa

                _ ->
                    text "No CPA"
            ]
        ]
