module Main exposing (..)

import Browser
import Element exposing (..)
import Element.Events as Events
import Element.Font as Font
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


actionsPerTurn : Int
actionsPerTurn =
    3


type TurnState
    = TurnOver
    | Actions Int


type alias GameId =
    String


type alias ConnectionId =
    String


type alias UserName =
    String


type alias PlayerList =
    List UserName


type GamePhase
    = NotStarted
    | ShowingHostOptions
    | ShowingConnectionOptions
    | ConnectedAsGuest
    | InGame Text TurnState
    | GameOver


type GameRole
    = Host PlayerList
    | Guest GameId


type alias Model =
    { gameId : GameId
    , gameRole : GameRole
    , gamePhase : GamePhase
    , textString : String
    }


init : String -> ( Model, Cmd Msg )
init gameId =
    ( { gameId = gameId
      , gameRole = Guest ""
      , gamePhase = NotStarted
      , textString = ""
      }
    , Cmd.none
    )



-- UPDATE


type Msg
    = InitHostGame
    | InitGuestGame
    | SetText String
    | SetHostIdForGuest GameId
    | GuestConnected UserName
    | ConnectedToHost GameId
    | ShowHostOptions
    | StartGame


makeToken : String -> Token
makeToken s =
    { content = s
    , state = Default
    }


makeText : String -> Text
makeText s =
    String.split "\n" s
        |> List.map (\line -> String.split " " line)
        |> List.map (List.map makeToken)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        InitHostGame ->
            ( { model | gameRole = Host [], gamePhase = ShowingHostOptions }
            , Ports.startHosting ()
            )

        InitGuestGame ->
            case model.gameRole of
                Host _ ->
                    -- TODO add error
                    ( model, Cmd.none )

                Guest hostId ->
                    ( { model | gamePhase = ShowingConnectionOptions }
                    , Ports.connectToHost hostId
                    )

        SetText s ->
            ( { model | textString = s }, Cmd.none )

        SetHostIdForGuest hostId ->
            ( { model | gameRole = Guest hostId }, Cmd.none )

        GuestConnected userName ->
            case model.gameRole of
                Host userNames ->
                    ( { model | gameRole = Host (userName :: userNames) }, Cmd.none )

                Guest _ ->
                    ( model, Cmd.none )

        ConnectedToHost hostId ->
            ( { model | gameRole = Guest hostId, gamePhase = ConnectedAsGuest }, Cmd.none )

        ShowHostOptions ->
            ( { model | gamePhase = ShowingHostOptions }, Cmd.none )

        StartGame ->
            ( { model | gamePhase = InGame (makeText model.textString) (Actions actionsPerTurn) }, Cmd.none )



-- VIEW


viewIntro : GameRole -> Html Msg
viewIntro gameRole =
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
                , text =
                    case gameRole of
                        Guest gameId ->
                            gameId

                        Host _ ->
                            "N/A"
                }
            ]


basePadding =
    { top = 0
    , right = 0
    , bottom = 0
    , left = 0
    }


viewHostOptions : String -> GameId -> Html Msg
viewHostOptions textString hostId =
    layout [ padding 20 ] <|
        column
            [ centerX, alignTop, spacing 20 ]
        <|
            [ el [ centerX ] (text "Game ID")
            , el [ centerX, Font.size 36, Font.family [ Font.monospace ] ] (text hostId)
            , Input.multiline
                [ centerX, width (px 600), paddingXY 0 20 ]
                { onChange = SetText
                , placeholder = Just (Input.placeholder [] (text "Poem starter text"))
                , text = textString
                , spellcheck = False
                , label =
                    Input.labelAbove
                        [ centerX ]
                        (text "Poem starter text")
                }
            , Input.button [ centerX ] { onPress = Just StartGame, label = text "Start game" }
            ]


viewConnectionOptions : Html Msg
viewConnectionOptions =
    layout [ padding 20 ] <|
        column
            [ centerX, alignTop ]
            [ text "Connection options" ]


viewGuestGameLobby : Html Msg
viewGuestGameLobby =
    layout [ padding 20 ] <|
        column
            [ centerX, alignTop ]
            [ text "Waiting for host to begin..." ]


view : Model -> Html Msg
view model =
    case model.gamePhase of
        NotStarted ->
            viewIntro model.gameRole

        ShowingHostOptions ->
            viewHostOptions model.textString model.gameId

        ShowingConnectionOptions ->
            viewConnectionOptions

        ConnectedAsGuest ->
            viewGuestGameLobby

        InGame _ _ ->
            viewIntro model.gameRole

        GameOver ->
            viewIntro model.gameRole



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Ports.guestConnected GuestConnected
        , Ports.connectedAsGuest ConnectedToHost
        ]
