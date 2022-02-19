module Main exposing (..)

import Browser
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)
import Ports exposing (..)



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


type GamePhase
    = NotStarted
    | ShowingConnectionOptions
    | InGame TurnState
    | GameOver


type alias Model =
    { isHost : Bool
    , gamePhase : GamePhase
    , text : Text
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { isHost = False, gamePhase = NotStarted, text = [] }, Cmd.none )



-- UPDATE


type Msg
    = StartGame
    | ConnectToGame


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        StartGame ->
            ( { model | isHost = True, gamePhase = ShowingConnectionOptions }, Cmd.none )

        ConnectToGame ->
            ( { model | isHost = False, gamePhase = ShowingConnectionOptions }, Cmd.none )



-- VIEW


viewIntro : Model -> Html Msg
viewIntro model =
    div []
        [ button [ onClick StartGame ] [ text "Start game" ]
        , button [ onClick ConnectToGame ] [ text "Connect to game" ]
        ]


viewConnectionOptions : Model -> Html Msg
viewConnectionOptions model =
    div []
        [ button [ onClick StartGame ] [ text "Start game" ]
        , button [ onClick ConnectToGame ] [ text "Connect to game" ]
        ]


view : Model -> Html Msg
view model =
    case model.gamePhase of
        NotStarted ->
            viewIntro model

        ShowingConnectionOptions ->
            viewIntro model

        InGame _ ->
            viewIntro model

        GameOver ->
            viewIntro model



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none
