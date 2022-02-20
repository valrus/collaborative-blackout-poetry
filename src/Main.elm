module Main exposing (..)

import Browser
import Element exposing (..)
import Element.Events as Events
import Element.Input as Input
import Html exposing (Html, div)
import Json.Decode as D
import Ports



-- MAIN


main =
    Browser.element { init = init, update = update, view = view, subscriptions = subscriptions }



-- MODEL


type TokenState
    = Default
    | Circled
    | Obscured


type alias Token =
    { content : String
    , state : TokenState
    }


type alias TextLine =
    List Token


type alias Text =
    List TextLine


type TurnState
    = TurnOver
    | Actions Int


type alias GameId =
    String


type alias ConnectionId =
    String


type GamePhase
    = NotStarted
    | ShowingHostOptions
    | ShowingConnectionOptions
    | InGame TurnState
    | GameOver


type GameRole
    = Host
    | Guest GameId


type alias Model =
    { gameId : GameId
    , gameRole : GameRole
    , gamePhase : GamePhase
    , text : Text
    }


init : String -> ( Model, Cmd Msg )
init gameId =
    ( { gameId = gameId, gameRole = Host, gamePhase = NotStarted, text = [] }
    , Cmd.none
    )



-- UPDATE


type Msg
    = InitHostGame
    | InitGuestGame
    | SetHostIdForGuest GameId
    | ConnectedAsGuest GameId
    | ShowHostOptions
    | StartGame


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        InitHostGame ->
            ( { model | gameRole = Host, gamePhase = ShowingHostOptions }
            , Cmd.none
            )

        InitGuestGame ->
            case model.gameRole of
                Host ->
                    -- TODO add error
                    ( model, Cmd.none )

                Guest hostId ->
                    ( { model | gamePhase = ShowingConnectionOptions }
                    , Ports.connectToHost hostId
                    )

        SetHostIdForGuest hostId ->
            ( { model | gameRole = Guest hostId }, Cmd.none )

        ConnectedAsGuest hostId ->
            ( model, Cmd.none )

        ShowHostOptions ->
            ( { model | gamePhase = ShowingHostOptions }, Cmd.none )

        StartGame ->
            ( model, Cmd.none )



-- VIEW


viewIntro : Model -> Html Msg
viewIntro model =
    layout [ padding 20 ] <|
        column
            [ centerX, alignTop, spacing 20 ]
            [ Input.button [ centerX ] { onPress = Just ShowHostOptions, label = text "Host game" }
            , Input.text
                [ centerX ]
                { label =
                    Input.labelBelow
                        [ centerX ]
                        (Input.button [] { onPress = Just InitGuestGame, label = text "Connect to game" })
                , onChange = SetHostIdForGuest
                , placeholder = Just (Input.placeholder [] (text "Game ID"))
                , text = ""
                }
            ]


viewHostOptions : String -> Html Msg
viewHostOptions hostId =
    layout [ padding 20 ] <|
        column
            [ centerX, alignTop ]
            [ text hostId ]


viewConnectionOptions : Html Msg
viewConnectionOptions =
    layout [ padding 20 ] <|
        column
            [ centerX, alignTop ]
            [ text "Connection options" ]


view : Model -> Html Msg
view model =
    case model.gamePhase of
        NotStarted ->
            viewIntro model

        ShowingHostOptions ->
            viewHostOptions model.gameId

        ShowingConnectionOptions ->
            viewConnectionOptions

        InGame _ ->
            viewIntro model

        GameOver ->
            viewIntro model



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none
