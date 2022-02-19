module Main exposing (..)

import Browser
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)
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
    , gameRole : Maybe GameRole
    , gamePhase : GamePhase
    , text : Text
    }


init : String -> ( Model, Cmd Msg )
init gameId =
    ( { gameId = gameId, gameRole = Nothing, gamePhase = NotStarted, text = [] }
    , Cmd.none
    )



-- UPDATE


type Msg
    = InitHostGame
    | InitGuestGame GameId
    | ConnectedAsGuest GameId
    | ShowHostOptions
    | StartGame


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        InitHostGame ->
            ( { model | gameRole = Just Host, gamePhase = ShowingHostOptions }
            , Cmd.none
            )

        InitGuestGame hostId ->
            ( { model | gameRole = Just (Guest hostId), gamePhase = ShowingConnectionOptions }
            , Ports.connectToHost hostId
            )

        ShowHostOptions ->
            ( { model | gamePhase = ShowingHostOptions }, Cmd.none )

        StartGame ->
            ( model, Cmd.none )



-- VIEW


viewIntro : Model -> Html Msg
viewIntro model =
    div []
        [ button [ onClick ShowHostOptions ] [ text "Host game" ]
        , button [ onClick (InitGuestGame "") ] [ text "Connect to game" ]
        ]


viewHostOptions : String -> Html Msg
viewHostOptions hostId =
    div [] [ text hostId ]


viewConnectionOptions : Html Msg
viewConnectionOptions =
    div [] [ text "Connection options" ]


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
