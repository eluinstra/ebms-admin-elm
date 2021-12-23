module Main exposing (main)

import About as About
import Bootstrap.Grid as Grid
import Bootstrap.Navbar as Navbar
import Browser exposing (UrlRequest)
import Browser.Navigation as Navigation
import Home as Home
import Html exposing (..)
import Html.Attributes exposing (..)
import Ping as Ping
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
    | PingPage Ping.Model
    | NotFound


type Route
    = Home
    | Ping
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
init _ url key =
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
    | HomeMsg Home.Msg
    | PingMsg Ping.Msg
    | AboutMsg About.Msg


subscriptions : Model -> Sub Msg
subscriptions model =
    Navbar.subscriptions model.navState NavMsg


toHome : Model -> ( Home.Model, Cmd Home.Msg ) -> ( Model, Cmd Msg )
toHome model ( homeModel, cmd ) =
    ( { model | page = HomePage homeModel }, Cmd.map HomeMsg cmd )


toPing : Model -> ( Ping.Model, Cmd Ping.Msg ) -> ( Model, Cmd Msg )
toPing model ( pingModel, cmd ) =
    ( { model | page = PingPage pingModel }, Cmd.map PingMsg cmd )


toAbout : Model -> ( About.Model, Cmd About.Msg ) -> ( Model, Cmd Msg )
toAbout model ( aboutModel, cmd ) =
    ( { model | page = AboutPage aboutModel }, Cmd.map AboutMsg cmd )


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
            ( { model | navState = state }, Cmd.none )

        HomeMsg homeMsg ->
            case model.page of
                HomePage homeModel ->
                    -- ( { model | page = homeModel |> HomePage }, Cmd.none )
                    Home.update homeMsg homeModel |> toHome model

                _ ->
                    ( model, Cmd.none )

        PingMsg pingMsg ->
            case model.page of
                PingPage pingModel ->
                    -- ( { model | page = pingModel |> PingPage }, Cmd.none )
                    Ping.update pingMsg pingModel |> toPing model

                _ ->
                    ( model, Cmd.none )

        AboutMsg aboutMsg ->
            case model.page of
                AboutPage aboutModel ->
                    -- ( { model | page = aboutModel |> AboutPage }, Cmd.none )
                    About.update aboutMsg aboutModel |> toAbout model

                _ ->
                    ( model, Cmd.none )


urlUpdate : Url -> Model -> ( Model, Cmd Msg )
urlUpdate url model =
    case decode url of
        Nothing ->
            ( { model | page = NotFound }, Cmd.none )

        Just Home ->
            -- ( { model | page = Tuple.first (Home.init ()) |> HomePage }, Cmd.none )
            Home.init () |> toHome model

        Just Ping ->
            -- ( { model | page = Tuple.first (Ping.init ()) |> PingPage }, Cmd.none )
            Ping.init () |> toPing model

        Just About ->
            -- ( { model | page = Tuple.first (About.init ()) |> AboutPage }, Cmd.none )
            About.init () |> toAbout model


decode : Url -> Maybe Route
decode url =
    { url | path = Maybe.withDefault "" url.fragment, fragment = Nothing } |> Parser.parse routeParser


routeParser : Parser (Route -> a) a
routeParser =
    Parser.oneOf
        [ Parser.map Home Parser.top
        , Parser.map Ping (Parser.s "ping")
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
            [ Navbar.dropdown
                { id = "cpasDropdown"
                , toggle = Navbar.dropdownToggle [] [ text "CPA Service" ]
                , items =
                    [ Navbar.dropdownItem
                        [ href "#" ]
                        [ text "CPAs" ]
                    , Navbar.dropdownDivider
                    , Navbar.dropdownItem
                        [ href "#" ]
                        [ text "Url Mappings" ]
                    , Navbar.dropdownDivider
                    , Navbar.dropdownItem
                        [ href "#" ]
                        [ text "Certificate Mappings" ]
                    ]
                }
            , Navbar.dropdown
                { id = "messagesDropdown"
                , toggle = Navbar.dropdownToggle [] [ text "Message Service" ]
                , items =
                    [ Navbar.dropdownItem
                        [ href "#ping" ]
                        [ text "Ping" ]
                    , Navbar.dropdownDivider
                    , Navbar.dropdownItem
                        [ href "#" ]
                        [ text "Unprocessed Messages" ]
                    , Navbar.dropdownDivider
                    , Navbar.dropdownItem
                        [ href "#" ]
                        [ text "Message Events" ]
                    , Navbar.dropdownDivider
                    , Navbar.dropdownItem
                        [ href "#" ]
                        [ text "Send Message" ]
                    , Navbar.dropdownItem
                        [ href "#" ]
                        [ text "Resend Message" ]
                    , Navbar.dropdownDivider
                    , Navbar.dropdownItem
                        [ href "#" ]
                        [ text "Message Status" ]
                    ]
                }
            , Navbar.itemLink [ href "#about" ] [ text "About" ]
            ]
        |> Navbar.view model.navState


mainContent : Model -> Html Msg
mainContent model =
    Grid.container [] <|
        case model.page of
            HomePage homeModel ->
                [ Home.view homeModel |> Html.map HomeMsg ]

            PingPage pingModel ->
                [ Ping.view pingModel |> Html.map PingMsg ]

            AboutPage aboutModel ->
                [ About.view aboutModel |> Html.map AboutMsg ]

            NotFound ->
                [ h1 [] [ text "Page Not Found" ] ]
