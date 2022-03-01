module Main exposing (..)

import Array
import Browser
import Element exposing (..)
import Element.Background as Background
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


type alias TokenPosition =
    -- lineIndex, tokenIndex
    ( Int, Int )


type alias Token =
    { content : String
    , state : TokenState
    }


type alias TextLine =
    Array.Array Token


type alias Poem =
    Array.Array TextLine


actionsPerTurn : Int
actionsPerTurn =
    3


type alias GameId =
    String


type alias ConnectionId =
    String


type alias PlayerName =
    String


type alias PlayerData =
    { name : PlayerName
    , actions : Int
    }


type alias OtherPlayersList =
    List Player


type alias AllPlayersList =
    List Player


type GameMessage
    = GuestJoined PlayerName
    | UpdatePlayerList AllPlayersList
    | GameAction Poem AllPlayersList
    | Disconnection (Maybe PlayerName)



-- TODO divide these by host/guest?


type GamePhase
    = NotStarted
    | ShowingHostOptions
    | ConnectingAsGuest
    | ConnectedAsGuest
    | InGame Poem
    | GameOver


type Player
    = Host PlayerData
    | Guest PlayerData (Maybe GameId)


type alias ConfirmResetFlag =
    Bool


type alias Model =
    { gameId : GameId
    , player : Player
    , otherPlayers : OtherPlayersList
    , gamePhase : GamePhase
    , textString : String
    , confirmReset : ConfirmResetFlag
    }


startingPlayer : Player
startingPlayer =
    Guest { name = "", actions = actionsPerTurn } Nothing


init : String -> ( Model, Cmd Msg )
init gameId =
    ( { gameId = gameId
      , player = startingPlayer
      , otherPlayers = []
      , gamePhase = NotStarted
      , textString = ""
      , confirmReset = False
      }
    , Ports.init gameId
    )



-- UPDATE


type HostMsg
    = ShowHostOptions
    | SetGameText String
    | StartGame


type GuestMsg
    = InitGuestGame
    | ConnectedToHost GameId


type Msg
    = HostMsg HostMsg
    | GuestMsg GuestMsg
    | ResetToIntro
    | ClearResetModal
    | SetHostIdForGuest GameId
    | SetUserName PlayerName
    | ReceivedGameMessage (Result D.Error GameMessage)
    | SetTokenState TokenPosition TokenState


makeToken : String -> Token
makeToken s =
    { content = s
    , state = Default
    }


makeText : String -> Poem
makeText s =
    (Array.fromList <| String.split "\n" s)
        |> Array.map (\line -> String.split " " line |> Array.fromList)
        |> Array.map (Array.map makeToken)


nameOfPlayer : Player -> String
nameOfPlayer =
    dataForPlayer >> .name


actionsForPlayer : Player -> Int
actionsForPlayer =
    dataForPlayer >> .actions


dataForPlayer : Player -> PlayerData
dataForPlayer player =
    case player of
        Host playerData ->
            playerData

        Guest playerData _ ->
            playerData


getOtherPlayers : AllPlayersList -> Player -> OtherPlayersList
getOtherPlayers allPlayers currentPlayer =
    List.filter (\player -> nameOfPlayer player /= nameOfPlayer currentPlayer) allPlayers


handleGameMessage : Model -> GameMessage -> Model
handleGameMessage model gameMsg =
    case gameMsg of
        GuestJoined playerName ->
            case model.player of
                Host myData ->
                    { model
                        | otherPlayers =
                            Guest { name = playerName, actions = actionsPerTurn } (Just model.gameId)
                                :: model.otherPlayers
                    }

                Guest _ _ ->
                    -- Shouldn't happen. TODO show an error of some kind?
                    model

        UpdatePlayerList allPlayers ->
            { model
                | otherPlayers =
                    List.filter
                        (\player -> nameOfPlayer player /= nameOfPlayer model.player)
                        allPlayers
            }

        GameAction poem allPlayers ->
            { model | gamePhase = InGame poem, otherPlayers = getOtherPlayers allPlayers model.player }

        Disconnection hostOrPlayerName ->
            case hostOrPlayerName of
                Just playerName ->
                    { model | otherPlayers = List.filter (\player -> nameOfPlayer player /= playerName) model.otherPlayers }

                Nothing ->
                    { model | gamePhase = NotStarted, player = startingPlayer }


encodePlayer : Player -> E.Value
encodePlayer player =
    let
        isHost =
            case player of
                Host _ ->
                    True

                Guest _ _ ->
                    False
    in
    E.object
        [ ( "name", E.string <| nameOfPlayer player )
        , ( "isHost", E.bool isHost )
        , ( "actions", E.int <| actionsForPlayer player )
        ]


encodeToken : Token -> E.Value
encodeToken token =
    E.object
        [ ( "content", E.string token.content )
        , ( "state"
          , case token.state of
                Default ->
                    E.string "default"

                Circled ->
                    E.string "circled"

                Obscured ->
                    E.string "obscured"
          )
        ]


encodePoem : Poem -> E.Value
encodePoem poem =
    E.list (\line -> E.list encodeToken (Array.toList line)) (Array.toList poem)


encodeGameMsg : GameMessage -> E.Value
encodeGameMsg gameMsg =
    case gameMsg of
        GuestJoined playerName ->
            E.object [ ( "guestName", E.string playerName ) ]

        UpdatePlayerList allPlayers ->
            E.list encodePlayer allPlayers

        GameAction poem otherPlayers ->
            E.object
                [ ( "poem", encodePoem poem )
                , ( "otherPlayers"
                  , E.list encodePlayer otherPlayers
                  )
                ]

        Disconnection hostOrPlayerName ->
            E.object [ ( "disconnection", Maybe.withDefault E.null (Maybe.map E.string hostOrPlayerName) ) ]


updateTokenState : TokenPosition -> TokenState -> Poem -> Poem
updateTokenState tokenPosition tokenState poem =
    let
        ( lineIndex, tokenIndex ) =
            tokenPosition
    in
    case Array.get lineIndex poem of
        Nothing ->
            poem

        Just line ->
            case Array.get tokenIndex line of
                Nothing ->
                    poem

                Just token ->
                    Array.set lineIndex (Array.set tokenIndex { token | state = tokenState } line) poem


handleHostMsg : HostMsg -> Model -> ( Model, Cmd Msg )
handleHostMsg hostMsg model =
    case hostMsg of
        ShowHostOptions ->
            ( { model
                | gamePhase = ShowingHostOptions
                , player = Host { name = nameOfPlayer model.player, actions = actionsPerTurn }
              }
            , Ports.startHosting ()
            )

        SetGameText s ->
            ( { model | textString = s }, Cmd.none )

        StartGame ->
            let
                poem =
                    makeText model.textString
            in
            ( { model
                | gamePhase = InGame poem
              }
            , Ports.sendAsHost (encodeGameMsg <| GameAction poem (getAllPlayers model))
            )


handleGuestMsg : GuestMsg -> Model -> ( Model, Cmd Msg )
handleGuestMsg guestMsg model =
    case guestMsg of
        InitGuestGame ->
            case model.player of
                Host _ ->
                    -- TODO add error
                    ( model, Cmd.none )

                Guest _ Nothing ->
                    -- TODO add error
                    ( model, Cmd.none )

                Guest playerData (Just gameId) ->
                    ( { model | gamePhase = ConnectingAsGuest }
                    , Ports.connectToHost gameId
                    )

        ConnectedToHost gameId ->
            let
                playerData =
                    case model.player of
                        Host data ->
                            data

                        Guest data _ ->
                            data
            in
            ( { model
                | player = Guest playerData (Just gameId)
                , gamePhase = ConnectedAsGuest
              }
            , Ports.sendAsGuest <| encodeGameMsg (GuestJoined <| nameOfPlayer model.player)
            )


sendForRole : Player -> (E.Value -> Cmd msg)
sendForRole player =
    case player of
        Host _ ->
            Ports.sendAsHost

        Guest _ _ ->
            Ports.sendAsGuest


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        -- intro screen changes
        SetUserName name ->
            let
                playerWithNewName =
                    case model.player of
                        Guest playerData gameId ->
                            Guest { playerData | name = name } gameId

                        Host playerData ->
                            Host { playerData | name = name }
            in
            ( { model | player = playerWithNewName }
              -- TODO send to other players
            , Cmd.none
            )

        SetHostIdForGuest gameId ->
            ( { model | player = Guest (dataForPlayer model.player) (Just gameId) }
            , Cmd.none
            )

        -- host/guest specific setup changes
        HostMsg hostMsg ->
            handleHostMsg hostMsg model

        GuestMsg guestMsg ->
            handleGuestMsg guestMsg model

        -- general navigation changes
        ResetToIntro ->
            let
                newModel =
                    case ( model.gamePhase, model.confirmReset ) of
                        ( InGame _, False ) ->
                            { model | confirmReset = True }

                        _ ->
                            { model | gamePhase = NotStarted, player = startingPlayer, confirmReset = False }
            in
            ( newModel
            , if newModel.confirmReset == True then
                Cmd.none

              else
                Ports.reset (Just <| nameOfPlayer model.player)
            )

        ClearResetModal ->
            ( { model | confirmReset = False }, Cmd.none )

        -- game state changes
        ReceivedGameMessage (Err err) ->
            -- TODO: handle somehow
            ( model, Cmd.none )

        ReceivedGameMessage (Ok gameMsg) ->
            -- TODO: maybe should only handle this if user is in game or lobby
            let
                newModel =
                    handleGameMessage model gameMsg
            in
            ( newModel
            , case ( newModel.player, gameMsg ) of
                ( Host _, GuestJoined playerName ) ->
                    -- forward the whole player list out
                    Ports.sendAsHost (encodeGameMsg <| UpdatePlayerList (getAllPlayers newModel))

                ( Host _, Disconnection playerName ) ->
                    -- forward the whole player list out
                    Ports.sendAsHost (encodeGameMsg <| UpdatePlayerList (getAllPlayers newModel))

                ( Host _, _ ) ->
                    -- forward the received message
                    Ports.sendAsHost (encodeGameMsg gameMsg)

                ( Guest _ _, Disconnection _ ) ->
                    -- disconnection from host, reset
                    Ports.reset Nothing

                ( Guest _ _, _ ) ->
                    Cmd.none
            )

        SetTokenState tokenPosition tokenState ->
            case model.gamePhase of
                InGame poem ->
                    let
                        newPoem =
                            updateTokenState tokenPosition tokenState poem
                    in
                    ( { model | gamePhase = InGame newPoem }
                    , sendForRole model.player (encodeGameMsg <| GameAction newPoem model.otherPlayers)
                    )

                _ ->
                    ( model, Cmd.none )



-- VIEW


basePadding =
    { top = 0
    , right = 0
    , bottom = 0
    , left = 0
    }


buttonStyles : Bool -> List (Element.Attribute Msg)
buttonStyles isEnabled =
    [ Border.color (rgb 0 0 0)
    , Border.width 1
    ]
        ++ roundedBoxStyles
        ++ (if isEnabled then
                []

            else
                [ Font.color (rgb 0.5 0.5 0.5) ]
           )


roundedBoxStyles : List (Element.Attribute Msg)
roundedBoxStyles =
    [ padding 10
    , Border.solid
    , Border.rounded 5
    ]


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


viewPlayerList : AllPlayersList -> Element Msg
viewPlayerList playerList =
    column [ spacing 5, alignLeft ]
        (el [] (text "Players")
            :: List.map
                (\player ->
                    let
                        textAttrs =
                            case player of
                                Host _ ->
                                    [ Font.bold ]

                                _ ->
                                    []
                    in
                    el textAttrs (text <| nameOfPlayer player)
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


isValidGameId : Maybe GameId -> Bool
isValidGameId =
    Maybe.map (\id -> List.all Char.isLower (String.toList id) && (String.length id == 20))
        >> Maybe.withDefault False


viewIntro : Player -> Html Msg
viewIntro player =
    let
        gameId =
            case player of
                Guest _ id ->
                    id

                Host _ ->
                    -- Shouldn't ever happen
                    Nothing

        validId =
            isValidGameId gameId
    in
    layout [ padding 20 ] <|
        column
            mainColumnStyles
            [ userNameInput <| nameOfPlayer player
            , conditionalButton
                { msg = HostMsg ShowHostOptions
                , isEnabled = not (String.isEmpty <| nameOfPlayer player)
                , labelText = "Host game"
                }
            , Input.text
                gameIdStyles
                { label =
                    Input.labelBelow
                        (centerX :: defaultFontStyles)
                        (conditionalButton
                            { msg = GuestMsg InitGuestGame
                            , isEnabled = validId && not (String.isEmpty <| nameOfPlayer player)
                            , labelText = "Connect to game"
                            }
                        )
                , onChange = SetHostIdForGuest
                , placeholder = Just (Input.placeholder [] (text "Game ID from host"))
                , text = Maybe.withDefault "" gameId
                }
            ]


minTextWords : Int
minTextWords =
    100


isValidPoemString : String -> Bool
isValidPoemString s =
    (String.words s |> List.length) > minTextWords


viewLeftSidebar : AllPlayersList -> Element.Attribute Msg
viewLeftSidebar allPlayers =
    onLeft
        (column
            [ width (px 100), spacing 20, padding 20, Font.family [ Font.sansSerif ] ]
            [ resetToIntroButton, viewPlayerList allPlayers ]
        )


viewHostOptions : String -> GameId -> AllPlayersList -> Html Msg
viewHostOptions textString gameId allPlayers =
    let
        validPoemText =
            isValidPoemString textString
    in
    layout [ padding 20 ] <|
        column
            (mainColumnStyles
                ++ [ viewLeftSidebar allPlayers
                   , spacing 60
                   ]
            )
            [ column [ spacing 20, centerX ]
                [ el [ centerX ] (text "Game ID")
                , el gameIdStyles (text gameId)
                ]
            , column [ spacing 20 ]
                [ Input.multiline
                    [ centerX, width (px 600) ]
                    { onChange = SetGameText >> HostMsg
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
                    , msg = HostMsg StartGame
                    , labelText = "Start game"
                    }
                ]
            ]


viewGuestLobby : GamePhase -> AllPlayersList -> Html Msg
viewGuestLobby gamePhase allPlayers =
    layout [ padding 20 ] <|
        column
            (viewLeftSidebar allPlayers
                :: mainColumnStyles
                ++ [ spacing 40 ]
            )
            [ el [ centerX ] <|
                case gamePhase of
                    ConnectedAsGuest ->
                        text "Waiting for host to begin..."

                    _ ->
                        text "Connecting..."
            ]


viewToken : Int -> Int -> Token -> Element Msg
viewToken lineIndex tokenIndex token =
    let
        baseAttributes =
            [ Events.onClick (SetTokenState ( lineIndex, tokenIndex ) nextTokenState)
            , pointer
            ]

        ( textOuterAttributes, textAttributes, nextTokenState ) =
            case token.state of
                Default ->
                    ( []
                    , []
                    , Circled
                    )

                Circled ->
                    ( [ Border.glow (rgb 1.0 0.5 0.5) 2 ]
                    , [ Border.innerGlow (rgb 1.0 0.5 0.5) 2 ]
                    , Obscured
                    )

                Obscured ->
                    ( []
                    , [ Font.color (rgb 1.0 1.0 1.0) ]
                    , Default
                    )
    in
    el
        -- We need an extra wrapper for both outer and inner glow; see
        -- https://github.com/mdgriffith/elm-ui/issues/18
        textOuterAttributes
        (el
            (baseAttributes ++ textAttributes)
            (text token.content)
        )


viewPoemLine : Int -> TextLine -> List (Element Msg)
viewPoemLine lineIndex line =
    List.intersperse (el [] (text " ")) <|
        List.indexedMap
            (viewToken lineIndex)
            (Array.toList line)


boxShadowStyles : List (Element.Attribute Msg)
boxShadowStyles =
    [ Border.shadow { offset = ( 1.0, 2.0 ), blur = 2.0, size = 2.0, color = rgba 0 0 0 0.333 }
    , Border.shadow { offset = ( 2.0, 4.0 ), blur = 4.0, size = 4.0, color = rgba 0 0 0 0.333 }
    , Border.shadow { offset = ( 3.0, 6.0 ), blur = 6.0, size = 6.0, color = rgba 0 0 0 0.333 }
    ]


viewConfirmModal : Player -> Element.Attribute Msg
viewConfirmModal player =
    let
        warningText =
            case player of
                Host _ ->
                    "Are you sure? This will end the game and disconnect all players."

                Guest _ _ ->
                    "Are you sure? This will leave the game."
    in
    inFront <|
        el [ centerX, centerY, width (px 400), height shrink ] <|
            column
                (roundedBoxStyles
                    ++ boxShadowStyles
                    ++ [ centerX, centerY, Background.color (rgb 1.0 1.0 1.0), spacing 20, padding 30 ]
                )
                [ paragraph [] [ text warningText ]
                , row [ width fill, spaceEvenly ]
                    [ Input.button
                        (buttonStyles True ++ [ Border.color (rgb 1.0 0.2 0.2) ])
                        { onPress = Just ResetToIntro, label = text "Yes, quit" }
                    , Input.button
                        (buttonStyles True)
                        { onPress = Just ClearResetModal, label = text "No, stay" }
                    ]
                ]


viewRightSidebar : Element.Attribute Msg
viewRightSidebar =
    onRight
        (column
            [ width (px 100), spacing 20, padding 20, Font.family [ Font.sansSerif ] ]
            []
        )


viewGame : Poem -> Model -> Html Msg
viewGame poem model =
    layout
        ([ padding 20
         ]
            ++ (if model.confirmReset then
                    [ viewConfirmModal model.player ]

                else
                    []
               )
        )
    <|
        Element.textColumn
            (viewLeftSidebar (getAllPlayers model)
                :: viewRightSidebar
                :: mainColumnStyles
                ++ [ spacing 10
                   , padding 10
                   , Font.family [ Font.serif ]
                   ]
            )
            (List.indexedMap
                (\i line ->
                    paragraph
                        []
                        (viewPoemLine i line)
                )
                (Array.toList poem)
            )


getAllPlayers : Model -> AllPlayersList
getAllPlayers model =
    model.player :: model.otherPlayers


view : Model -> Html Msg
view model =
    case model.gamePhase of
        NotStarted ->
            viewIntro model.player

        ShowingHostOptions ->
            viewHostOptions model.textString model.gameId (getAllPlayers model)

        ConnectingAsGuest ->
            viewGuestLobby model.gamePhase (getAllPlayers model)

        ConnectedAsGuest ->
            viewGuestLobby model.gamePhase (getAllPlayers model)

        InGame poem ->
            viewGame poem model

        GameOver ->
            viewIntro model.player



-- SUBSCRIPTIONS


guestJoinedDecoder : D.Decoder GameMessage
guestJoinedDecoder =
    D.map GuestJoined <| D.field "guestName" D.string


playerDataDecoder : D.Decoder PlayerData
playerDataDecoder =
    D.map2 PlayerData
        (D.field "name" D.string)
        (D.field "actions" D.int)


playerDecoder : D.Decoder Player
playerDecoder =
    D.field "isHost" D.bool
        |> D.andThen
            (\isHost ->
                case isHost of
                    False ->
                        D.map2 Guest playerDataDecoder (D.succeed Nothing)

                    True ->
                        D.map Host playerDataDecoder
            )


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
    D.array (D.array tokenDecoder)


gameActionDecoder : D.Decoder GameMessage
gameActionDecoder =
    D.map2 GameAction
        (D.at [ "poem" ] poemDecoder)
        (D.at [ "otherPlayers" ] (D.list playerDecoder))


disconnectionDecoder : D.Decoder GameMessage
disconnectionDecoder =
    D.map Disconnection <| D.field "disconnection" (D.maybe D.string)


gameMessageDecoder : D.Decoder GameMessage
gameMessageDecoder =
    D.oneOf
        [ guestJoinedDecoder -- PlayerName
        , updatePlayerListDecoder -- PlayerList
        , gameActionDecoder -- Poem PlayerList
        , disconnectionDecoder -- Maybe PlayerName
        ]


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Ports.connectedAsGuest (ConnectedToHost >> GuestMsg)
        , Ports.receivedMessage (ReceivedGameMessage << D.decodeValue gameMessageDecoder)
        , Ports.disconnect (\_ -> ResetToIntro)
        ]
