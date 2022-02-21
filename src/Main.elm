module Main exposing (..)

import Browser
import Debug
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


type alias Poem =
    List TextLine


actionsPerTurn : Int
actionsPerTurn =
    3


type alias GameId =
    String


type alias ConnectionId =
    String


type alias PlayerName =
    String


type alias Player =
    { name : PlayerName
    , isHost : Bool
    , actions : Int
    }


type alias PlayerList =
    List Player


type GameMessage
    = GuestJoined PlayerName
    | UpdatePlayerList PlayerList
    | GameAction Poem PlayerList



-- TODO divide these by host/guest?


type GamePhase
    = NotStarted
    | ShowingHostOptions
    | ConnectingAsGuest
    | ConnectedAsGuest
    | InGame Poem
    | GameOver


type GameRole
    = Host
    | Guest GameId


type alias Model =
    { gameId : GameId
    , gameRole : GameRole
    , userName : PlayerName
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
    = InitGuestGame
    | ResetToIntro
    | SetGameText String
    | SetHostIdForGuest GameId
    | SetUserName PlayerName
    | ConnectedToHost GameId
    | ShowHostOptions
    | StartGame
    | ReceivedGameMessage (Result D.Error GameMessage)


makeToken : String -> Token
makeToken s =
    { content = s
    , state = Default
    }


makeText : String -> Poem
makeText s =
    String.split "\n" s
        |> List.map (\line -> String.split " " line)
        |> List.map (List.map makeToken)


handleGameMessage : Model -> GameMessage -> ( Model, Cmd Msg )
handleGameMessage model gameMsg =
    case gameMsg of
        GuestJoined playerName ->
            case model.gameRole of
                Host ->
                    let
                        newPlayerList =
                            { name = Debug.log "New guest" playerName, isHost = False, actions = actionsPerTurn }
                                :: model.playerList
                    in
                    ( { model | playerList = newPlayerList }, Cmd.none )

                Guest _ ->
                    -- Shouldn't happen
                    ( Debug.log "im guest?" model, Cmd.none )

        UpdatePlayerList playerList ->
            ( model, Cmd.none )

        GameAction poem playerList ->
            ( model, Cmd.none )


encodePlayer : Player -> E.Value
encodePlayer player =
    E.object
        [ ( "name", E.string player.name )
        , ( "isHost", E.bool player.isHost )
        , ( "actions", E.int player.actions )
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        InitGuestGame ->
            case model.gameRole of
                Host ->
                    -- TODO add error
                    ( model, Cmd.none )

                Guest hostId ->
                    ( { model | gamePhase = ConnectingAsGuest }
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

        ConnectedToHost hostId ->
            ( { model | gameRole = Guest hostId, gamePhase = ConnectedAsGuest }
            , Ports.sendAsGuest (E.object [ ( "guestName", E.string model.userName ) ])
            )

        ShowHostOptions ->
            ( { model
                | gamePhase = ShowingHostOptions
                , gameRole = Host
                , playerList = [ { name = model.userName, isHost = True, actions = actionsPerTurn } ]
              }
            , Ports.startHosting ()
            )

        StartGame ->
            ( { model
                | gamePhase = InGame (makeText model.textString)
              }
            , Cmd.none
            )

        ReceivedGameMessage (Err err) ->
            ( model, Cmd.none )

        ReceivedGameMessage (Ok gameMsg) ->
            handleGameMessage model gameMsg



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
                (text "Your name")
        , onChange = SetUserName
        , placeholder = Just (Input.placeholder [] (text "Enter name to start"))
        , text = userName
        }


resetToIntroButton : Element Msg
resetToIntroButton =
    Input.button (buttonStyles True) { onPress = Just ResetToIntro, label = text "Back" }


viewPlayerList : PlayerList -> Element Msg
viewPlayerList playerList =
    column [ spacing 5, alignLeft ]
        (el [] (text "Players")
            :: List.map
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


viewIntro : PlayerName -> GameRole -> Html Msg
viewIntro playerName gameRole =
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
            [ userNameInput playerName
            , conditionalButton
                { msg = ShowHostOptions
                , isEnabled = not (String.isEmpty playerName)
                , labelText = "Host game"
                }
            , Input.text
                gameIdStyles
                { label =
                    Input.labelBelow
                        (centerX :: defaultFontStyles)
                        (conditionalButton
                            { msg = InitGuestGame
                            , isEnabled = validId && not (String.isEmpty playerName)
                            , labelText = "Connect to game"
                            }
                        )
                , onChange = SetHostIdForGuest
                , placeholder = Just (Input.placeholder [] (text "Enter Game ID to connect to"))
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
            (mainColumnStyles
                ++ [ onLeft
                        (column
                            [ width (px 100), spacing 20 ]
                            [ resetToIntroButton, viewPlayerList playerList ]
                        )
                   , spacing 60
                   ]
            )
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


viewGuestLobby : GamePhase -> PlayerName -> Html Msg
viewGuestLobby gamePhase userName =
    layout [ padding 20 ] <|
        column
            (onLeft resetToIntroButton :: mainColumnStyles ++ [ spacing 40 ])
            [ el [ centerX ] <|
                case gamePhase of
                    ConnectedAsGuest ->
                        text "Waiting for host to begin..."

                    _ ->
                        text "Connecting..."
            ]


poemLine : TextLine -> List (Element Msg)
poemLine line =
    List.intersperse (el [] (text " ")) <| List.map (\token -> el [] (text token.content)) line


viewGame : Poem -> Html Msg
viewGame poem =
    layout [ padding 20 ] <|
        Element.textColumn (onLeft resetToIntroButton :: mainColumnStyles ++ [ spacing 10, padding 10 ])
            (List.map (\line -> paragraph [] (poemLine line)) poem)


view : Model -> Html Msg
view model =
    case model.gamePhase of
        NotStarted ->
            viewIntro model.userName model.gameRole

        ShowingHostOptions ->
            viewHostOptions model.textString model.gameId model.playerList

        ConnectingAsGuest ->
            viewGuestLobby model.gamePhase model.userName

        ConnectedAsGuest ->
            viewGuestLobby model.gamePhase model.userName

        InGame poem ->
            viewGame poem

        GameOver ->
            viewIntro model.userName model.gameRole



-- SUBSCRIPTIONS


guestJoinedDecoder : D.Decoder GameMessage
guestJoinedDecoder =
    D.map GuestJoined <| D.field "guestName" D.string


playerDecoder : D.Decoder Player
playerDecoder =
    D.map3 Player
        (D.field "name" D.string)
        (D.field "isHost" D.bool)
        (D.field "actions" D.int)


updatePlayerListDecoder : D.Decoder GameMessage
updatePlayerListDecoder =
    D.map UpdatePlayerList <|
        D.list playerDecoder


tokenDecoder : D.Decoder Token
tokenDecoder =
    D.map2 Token
        (D.field "content" D.string)
        (D.field "state" D.string
            |> D.andThen
                (\stateString ->
                    case stateString of
                        "default" ->
                            D.succeed Default

                        "circled" ->
                            D.succeed Circled

                        "obscured" ->
                            D.succeed Obscured

                        _ ->
                            D.fail "Invalid state string"
                )
        )


poemDecoder : D.Decoder Poem
poemDecoder =
    D.list (D.list tokenDecoder)


gameActionDecoder : D.Decoder GameMessage
gameActionDecoder =
    D.map2 GameAction
        (D.at [ "poem" ] poemDecoder)
        (D.at [ "playerList" ] (D.list playerDecoder))


gameMessageDecoder : D.Decoder GameMessage
gameMessageDecoder =
    D.oneOf
        [ guestJoinedDecoder -- PlayerName
        , updatePlayerListDecoder -- PlayerList
        , gameActionDecoder -- Poem PlayerList
        ]


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Ports.connectedAsGuest ConnectedToHost
        , Ports.receivedMessage (ReceivedGameMessage << D.decodeValue gameMessageDecoder)
        ]
