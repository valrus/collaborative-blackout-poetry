module Main exposing (..)

import Browser
import Element exposing (..)
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Element.Input as Input
import Html exposing (Html, div)
import Json.Decode as D
import Json.Encode as E
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


type alias User =
    { name : UserName
    , isHost : Bool
    }


type alias PlayerList =
    List User


type alias GameState =
    { poem : Text
    , turn : TurnState
    }



-- TODO divide these by host/guest?


type GamePhase
    = NotStarted
    | ShowingHostOptions
    | ShowingConnectionOptions
    | ConnectedAsGuest
    | InGame GameState
    | GameOver


type GameRole
    = Host
    | Guest GameId


type alias Model =
    { gameId : GameId
    , gameRole : GameRole
    , userName : UserName
    , playerList : PlayerList
    , gamePhase : GamePhase
    , textString : String
    }


init : String -> ( Model, Cmd Msg )
init gameId =
    ( { gameId = gameId
      , gameRole = Guest ""
      , userName = ""
      , playerList = []
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
            ( { model | gameRole = Host, gamePhase = ShowingHostOptions }
            , Ports.startHosting ()
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
                Host ->
                    -- TODO send to other players
                    ( { model | gameRole = Host, playerList = { name = userName, isHost = False } :: model.playerList }, Cmd.none )

                Guest _ ->
                    -- TODO error?
                    ( model, Cmd.none )

        ConnectedToHost hostId ->
            ( { model | gameRole = Guest hostId, gamePhase = ConnectedAsGuest }
            , Ports.sendAsGuest E.null
            )

        ShowHostOptions ->
            ( { model | gamePhase = ShowingHostOptions }, Cmd.none )

        StartGame ->
            ( { model
                | gamePhase = InGame { poem = makeText model.textString, turn = Actions actionsPerTurn }
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


buttonStyles : Bool -> List (Element.Attribute Msg)
buttonStyles isEnabled =
    [ padding 10
    , Border.solid
    , Border.width 1
    , Border.rounded 5
    , Border.color (rgb 0 0 0)
    ]
        ++ (if isEnabled then
                []

            else
                [ Font.color (rgb 0.5 0.5 0.5) ]
           )


conditionalButton : { isEnabled : Bool, msg : Msg, labelText : String } -> Element Msg
conditionalButton opts =
    Input.button
        (centerX :: buttonStyles opts.isEnabled)
        { onPress =
            if opts.isEnabled then
                Just opts.msg

            else
                Nothing
        , label = text opts.labelText
        }


gameIdStyles : List (Element.Attribute Msg)
gameIdStyles =
    [ centerX, Font.size 36, Font.family [ Font.monospace ] ]


userNameInput : String -> Element Msg
userNameInput userName =
    Input.text
        [ centerX, width (300 |> px) ]
        { label =
            Input.labelAbove
                [ centerX ]
                (text "User name")
        , onChange = SetUserName
        , placeholder = Nothing
        , text = userName
        }


resetToIntroButton : Element.Attribute Msg
resetToIntroButton =
    onLeft (Input.button (buttonStyles True) { onPress = Just ResetToIntro, label = text "Back" })


showPlayerList : PlayerList -> Element.Attribute Msg
showPlayerList playerList =
    onRight
        (column [ spacing 5 ]
            (List.map
                (\player ->
                    el
                        (if player.isHost then
                            [ Font.bold ]

                         else
                            []
                        )
                        (text player.name)
                )
                playerList
            )
        )


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


isValidGameId : GameId -> Bool
isValidGameId gameId =
    List.all Char.isLower (String.toList gameId) && (String.length gameId == 20)


viewIntro : GameRole -> Html Msg
viewIntro gameRole =
    let
        gameId =
            case gameRole of
                Guest id ->
                    id

                Host ->
                    -- Shouldn't ever happen
                    "N/A"

        validId =
            isValidGameId gameId
    in
    layout [ padding 20 ] <|
        column
            mainColumnStyles
            [ Input.button
                (centerX :: buttonStyles True)
                { onPress = Just ShowHostOptions, label = text "Host game" }
            , Input.text
                gameIdStyles
                { label =
                    Input.labelBelow
                        (centerX :: defaultFontStyles)
                        (conditionalButton
                            { msg = InitGuestGame
                            , isEnabled = validId
                            , labelText = "Connect to game"
                            }
                        )
                , onChange = SetHostIdForGuest
                , placeholder = Just (Input.placeholder [] (text "Game ID"))
                , text = gameId
                }
            ]


minTextWords : Int
minTextWords =
    100


isValidPoemString : String -> Bool
isValidPoemString s =
    (String.words s |> List.length) > minTextWords


viewHostOptions : String -> GameId -> PlayerList -> Html Msg
viewHostOptions textString hostId playerList =
    let
        validPoemText =
            isValidPoemString textString
    in
    layout [ padding 20 ] <|
        column
            (mainColumnStyles ++ [ resetToIntroButton, showPlayerList playerList, spacing 60 ])
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
                , conditionalButton
                    { isEnabled = isValidPoemString textString
                    , msg = StartGame
                    , labelText = "Start game"
                    }
                ]
            ]


viewGuestLobby : GamePhase -> UserName -> Html Msg
viewGuestLobby gamePhase userName =
    layout [ padding 20 ] <|
        column
            (resetToIntroButton :: mainColumnStyles ++ [ spacing 40 ])
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
            viewHostOptions model.textString model.gameId model.playerList

        ShowingConnectionOptions ->
            viewGuestLobby model.gamePhase model.userName

        ConnectedAsGuest ->
            viewGuestLobby model.gamePhase model.userName

        InGame gameState ->
            viewGame gameState.poem

        GameOver ->
            viewIntro model.gameRole



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Ports.guestConnected GuestConnected
        , Ports.connectedAsGuest ConnectedToHost
        ]
