module Main exposing (..)

import Browser
import Element exposing (..)
import Element.Border as Border
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



-- TODO divide these by host/guest?


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
    , userName : UserName
    , gamePhase : GamePhase
    , textString : String
    }


init : String -> ( Model, Cmd Msg )
init gameId =
    ( { gameId = gameId
      , gameRole = Guest ""
      , userName = ""
      , gamePhase = NotStarted
      , textString = ""
      }
    , Cmd.none
    )



-- UPDATE


type Msg
    = InitHostGame
    | InitGuestGame
    | ResetToIntro
    | SetGameText String
    | SetHostIdForGuest GameId
    | SetUserName UserName
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

        ResetToIntro ->
            ( { model | gamePhase = NotStarted, gameRole = Guest "" }
            , Cmd.none
            )

        SetUserName name ->
            ( { model | userName = name }
              -- TODO send to other players
            , Cmd.none
            )

        SetGameText s ->
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
            ( { model
                | gamePhase = InGame (makeText model.textString) (Actions actionsPerTurn)
              }
            , Cmd.none
            )



-- VIEW


basePadding =
    { top = 0
    , right = 0
    , bottom = 0
    , left = 0
    }


buttonStyles : List (Element.Attribute Msg)
buttonStyles =
    [ padding 10
    , Border.solid
    , Border.width 1
    , Border.rounded 5
    , Border.color (rgb 0 0 0)
    ]


gameIdStyles : List (Element.Attribute Msg)
gameIdStyles =
    [ centerX, Font.size 36, Font.family [ Font.monospace ] ]


userNameInput : String -> Element Msg
userNameInput userName =
    Input.text
        [ centerX ]
        { label =
            Input.labelBelow
                [ centerX ]
                (text "User name")
        , onChange = SetUserName
        , placeholder = Just (Input.placeholder [] (text "Enter name here"))
        , text = userName
        }


resetToIntroButton : Element.Attribute Msg
resetToIntroButton =
    onLeft (Input.button buttonStyles { onPress = Just ResetToIntro, label = text "Back" })


defaultFontStyles : List (Element.Attribute Msg)
defaultFontStyles =
    [ Font.size 20
    , Font.family
        [ Font.sansSerif ]
    , Font.color (rgb 0 0 0)
    ]


mainColumnStyles : List (Element.Attribute Msg)
mainColumnStyles =
    [ centerX
    , alignTop
    , spacing 20
    , width (px 600)
    ]


viewIntro : GameRole -> Html Msg
viewIntro gameRole =
    layout [ padding 20 ] <|
        column
            mainColumnStyles
            [ Input.button (centerX :: buttonStyles) { onPress = Just ShowHostOptions, label = text "Host game" }
            , Input.text
                gameIdStyles
                { label =
                    Input.labelBelow
                        (centerX :: defaultFontStyles)
                        (Input.button buttonStyles { onPress = Just InitGuestGame, label = text "Connect to game" })
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


viewHostOptions : String -> GameId -> Html Msg
viewHostOptions textString hostId =
    layout [ padding 20 ] <|
        column
            (mainColumnStyles ++ [ resetToIntroButton, spacing 60 ])
            [ column [ spacing 20, centerX ]
                [ el [ centerX ] (text "Game ID")
                , el gameIdStyles (text hostId)
                ]
            , column [ spacing 20 ]
                [ Input.multiline
                    [ centerX, width (px 600) ]
                    { onChange = SetGameText
                    , placeholder = Nothing
                    , text = textString
                    , spellcheck = False
                    , label =
                        Input.labelAbove
                            [ centerX ]
                            (text "Poem starter text")
                    }
                , Input.button (centerX :: buttonStyles) { onPress = Just StartGame, label = text "Start game" }
                ]
            ]


viewGuestLobby : GamePhase -> UserName -> Html Msg
viewGuestLobby gamePhase userName =
    layout [ padding 20 ] <|
        column
            (resetToIntroButton :: mainColumnStyles)
            [ userNameInput userName
            , el [ centerX ] <|
                case gamePhase of
                    ConnectedAsGuest ->
                        text "Waiting for host to begin..."

                    _ ->
                        text "Connecting..."
            ]


poemLine : TextLine -> List (Element Msg)
poemLine line =
    List.intersperse (el [] (text " ")) <| List.map (\token -> el [] (text token.content)) line


viewGame : Text -> Html Msg
viewGame poem =
    layout [ padding 20 ] <|
        Element.textColumn (resetToIntroButton :: mainColumnStyles ++ [ spacing 10, padding 10 ])
            (List.map (\line -> paragraph [] (poemLine line)) poem)


view : Model -> Html Msg
view model =
    case model.gamePhase of
        NotStarted ->
            viewIntro model.gameRole

        ShowingHostOptions ->
            viewHostOptions model.textString model.gameId

        ShowingConnectionOptions ->
            viewGuestLobby model.gamePhase model.userName

        ConnectedAsGuest ->
            viewGuestLobby model.gamePhase model.userName

        InGame text _ ->
            viewGame text

        GameOver ->
            viewIntro model.gameRole



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Ports.guestConnected GuestConnected
        , Ports.connectedAsGuest ConnectedToHost
        ]
