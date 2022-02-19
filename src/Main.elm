module Main exposing (..)

import Browser
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)
import Json.Decode as D
import Ports
import Random
import Random.Char
import Random.String



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


type alias HostId =
    String


type GamePhase
    = NotStarted
    | ShowingHostOptions HostId
    | ShowingConnectionOptions
    | InGame TurnState
    | GameOver


type alias Model =
    { isHost : Bool
    , connectionId : Maybe HostId
    , gamePhase : GamePhase
    , text : Text
    }


randomGameId =
    Random.String.string 20 Random.Char.english


init : ( Model, Cmd Msg )
init =
    ( { isHost = False, connectionId = Nothing, gamePhase = NotStarted, text = [] }
    , Random.generate SetGameId randomGameId
    )



-- UPDATE


type Msg
    = SetGameId
    | InitHostGame
    | InitGuestGame HostId
    | ShowHostOptions HostId
    | StartGame


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        InitHostGame ->
            ( { model | isHost = True, gamePhase = ShowingHostOptions }
            , Ports.startHosting ()
            )

        InitGuestGame hostId ->
            ( { model | isHost = False, gamePhase = ShowingConnectionOptions }
            , Ports.connectToHost hostId
            )



-- VIEW


viewIntro : Model -> Html Msg
viewIntro model =
    div []
        [ button [ onClick ShowHostOptions ] [ text "Host game" ]
        , button [ onClick (InitGuestGame "") ] [ text "Connect to game" ]
        ]


viewHostOptions : String -> Html Msg
viewHostOptions hostId =
    div []
        [ button [ onClick HostGame ] [ text hostId ] ]


viewConnectionOptions : Html Msg
viewConnectionOptions =
    div []
        [ button [ onClick HostGame ] [ text "Connection options" ] ]


view : Model -> Html Msg
view model =
    case model.gamePhase of
        NotStarted ->
            viewIntro model

        ShowingHostOptions hostId ->
            viewHostOptions hostId

        ShowingConnectionOptions ->
            viewConnectionOptions

        InGame _ ->
            viewIntro model

        GameOver ->
            viewIntro model



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Ports.connectionDescription (SetPeerId << D.decodeValue D.string) ]
