module Main exposing (main)

import About as About
import Bootstrap.Button as Button
import Bootstrap.Card as Card
import Bootstrap.Card.Block as Block
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Bootstrap.ListGroup as Listgroup
import Bootstrap.Modal as Modal
import Bootstrap.Navbar as Navbar
import Browser exposing (UrlRequest)
import Browser.Navigation as Navigation
import Home as Home
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Url exposing (Url)
import Url.Parser as Parser exposing ((</>), Parser)


type alias Flags =
    {}


type alias Model =
    { navKey : Navigation.Key
    , page : Page
    , navState : Navbar.State
    }


type Page
    = HomePage Home.Model
    | AboutPage About.Model
    | NotFound


type Route
    = Home
    | About


main : Program Flags Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlRequest = ClickedLink
        , onUrlChange = UrlChange
        }


init : Flags -> Url -> Navigation.Key -> ( Model, Cmd Msg )
init flags url key =
    let
        ( navState, navCmd ) =
            Navbar.initialState NavMsg

        ( model, urlCmd ) =
            urlUpdate url { navKey = key, navState = navState, page = NotFound }
    in
    ( model, Cmd.batch [ urlCmd, navCmd ] )


type Msg
    = UrlChange Url
    | ClickedLink UrlRequest
    | NavMsg Navbar.State


subscriptions : Model -> Sub Msg
subscriptions model =
    Navbar.subscriptions model.navState NavMsg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ClickedLink req ->
            case req of
                Browser.Internal url ->
                    ( model, Navigation.pushUrl model.navKey <| Url.toString url )

                Browser.External href ->
                    ( model, Navigation.load href )

        UrlChange url ->
            urlUpdate url model

        NavMsg state ->
            ( { model | navState = state }
            , Cmd.none
            )


urlUpdate : Url -> Model -> ( Model, Cmd Msg )
urlUpdate url model =
    case decode url of
        Nothing ->
            ( { model | page = NotFound }, Cmd.none )

        Just Home ->
            ( { model | page = Tuple.first (Home.init ()) |> HomePage }, Cmd.none )

        Just About ->
            ( { model | page = Tuple.first (About.init ()) |> AboutPage }, Cmd.none )


decode : Url -> Maybe Route
decode url =
    { url | path = Maybe.withDefault "" url.fragment, fragment = Nothing }
        |> Parser.parse routeParser


routeParser : Parser (Route -> a) a
routeParser =
    Parser.oneOf
        [ Parser.map Home Parser.top
        , Parser.map About (Parser.s "about")
        ]


view : Model -> Browser.Document Msg
view model =
    { title = "EbMS Admin"
    , body =
        [ div []
            [ menu model
            , mainContent model
            ]
        ]
    }


menu : Model -> Html Msg
menu model =
    Navbar.config NavMsg
        |> Navbar.withAnimation
        |> Navbar.container
        |> Navbar.brand [ href "#" ] [ img [ src "images/ebms_admin.gif" ] [] ]
        |> Navbar.items
            [ Navbar.itemLink [ href "#getting-started" ] [ text "Getting started" ]
            , Navbar.itemLink [ href "#about" ] [ text "About" ]
            ]
        |> Navbar.view model.navState


mainContent : Model -> Html Msg
mainContent model =
    Grid.container [] <|
    case model.page of
        HomePage homeModel ->
            -- Home.view homeModel
            [text "HomePage"]

        AboutPage aboutModel ->
            [text "About"]

        NotFound ->
            [text "Page Not Found"]



-- pageNotFound : List (Html Msg)
-- pageNotFound =
--     [ h1 [] [ text "Not found" ]
--     , text "SOrry couldn't find that page"
--     ]
