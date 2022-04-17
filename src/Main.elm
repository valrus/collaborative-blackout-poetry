module Main exposing (..)

import Animation
import Array
import Browser
import Html exposing (Html)
import Json.Decode as D
import Json.Encode as E
import Ports
import State exposing (..)
import Subscriptions exposing (subscriptions)
import Time exposing (millisToPosix)
import View exposing (..)



-- MAIN


main =
    Browser.element { init = init, update = update, view = view, subscriptions = subscriptions }



-- UPDATE


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


replenishActions : Player -> Player
replenishActions player =
    case player of
        Host data ->
            Host { data | actions = actionsPerTurn }

        Guest data gameId ->
            Guest { data | actions = actionsPerTurn } gameId


allActionsDepleted : AllPlayersList -> Bool
allActionsDepleted =
    List.all (\player -> actionCountForPlayer player == 0)


resetActionsIfNecessary : Model -> Poem -> AllPlayersList -> Model
resetActionsIfNecessary model poem allPlayers =
    if allActionsDepleted allPlayers then
        { model
            | gamePhase = InGame poem
            , player = replenishActions model.player
            , otherPlayers = List.map replenishActions <| getOtherPlayers allPlayers model.player
        }

    else
        { model
            | gamePhase = InGame poem
            , otherPlayers = getOtherPlayers allPlayers model.player
        }


playerMatches : Player -> Player -> Bool
playerMatches player otherPlayer =
    nameOfPlayer player == nameOfPlayer otherPlayer


selfAndOtherPlayers : Player -> AllPlayersList -> ( Maybe Player, OtherPlayersList )
selfAndOtherPlayers self allPlayers =
    List.partition (playerMatches self) allPlayers |> Tuple.mapFirst List.head


handleGameMessage : Model -> GameMessage -> Model
handleGameMessage model gameMsg =
    case gameMsg of
        GuestJoined playerName ->
            case model.player of
                Host myData ->
                    { model
                        | otherPlayers =
                            Guest
                                { name = playerName
                                , actions = actionsPerTurn
                                }
                                (Just model.gameId)
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
            case model.player of
                Host _ ->
                    resetActionsIfNecessary model poem allPlayers

                Guest _ _ ->
                    case selfAndOtherPlayers model.player allPlayers of
                        ( Just self, _ ) ->
                            { model
                                | gamePhase = InGame poem
                                , player = self
                                , otherPlayers = getOtherPlayers allPlayers model.player
                            }

                        ( Nothing, _ ) ->
                            -- a message about a game state not including this player?
                            -- let's not do anything with that!
                            model

        Disconnection hostOrPlayerName ->
            case hostOrPlayerName of
                Just playerName ->
                    { model | otherPlayers = List.filter (\player -> nameOfPlayer player /= playerName) model.otherPlayers }

                Nothing ->
                    { model | gamePhase = NotStarted, player = startingPlayer }


encodeActions : PlayerActions -> E.Value
encodeActions actions =
    case actions of
        RemainingActions count ->
            E.int count

        Passed ->
            E.null


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
        , ( "actions", encodeActions <| actionsForPlayer player )
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

                allPlayers =
                    getAllPlayers newModel
            in
            ( newModel
            , case ( newModel.player, gameMsg ) of
                ( Host _, GuestJoined playerName ) ->
                    -- forward the whole player list out
                    Ports.sendAsHost (encodeGameMsg <| UpdatePlayerList allPlayers)

                ( Host _, Disconnection playerName ) ->
                    -- forward the whole player list out
                    Ports.sendAsHost (encodeGameMsg <| UpdatePlayerList allPlayers)

                ( Host _, GameAction poem allPlayersList ) ->
                    -- forward the received message
                    Ports.sendAsHost (encodeGameMsg <| GameAction poem allPlayers)

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

                        modelWithActionDeducted =
                            { model | player = deductAction model.player }

                        newModel =
                            case model.player of
                                Host _ ->
                                    resetActionsIfNecessary
                                        modelWithActionDeducted
                                        newPoem
                                        (getAllPlayers modelWithActionDeducted)

                                Guest _ _ ->
                                    -- guests should never replenish actions themselves but wait
                                    -- for a message from the host
                                    modelWithActionDeducted
                    in
                    ( newModel
                    , sendForRole
                        newModel.player
                        (encodeGameMsg <| GameAction newPoem (getAllPlayers newModel))
                    )

                _ ->
                    ( model, Cmd.none )

        PassTurn ->
            case model.gamePhase of
                InGame poem ->
                    let
                        newPlayer =
                            case model.player of
                                Host data ->
                                    Host { data | actions = Passed }

                                Guest data gameId ->
                                    Guest { data | actions = Passed } gameId

                        newModel =
                            { model | player = newPlayer }
                    in
                    ( newModel
                    , sendForRole newPlayer (encodeGameMsg <| GameAction poem (getAllPlayers newModel))
                    )

                _ ->
                    ( model, Cmd.none )

        SetGameAction gameAction ->
            ( { model | gameAction = gameAction }, Cmd.none )

        FlashMessage message ->
            let
                toast =
                    model.toast

                newStyle =
                    Animation.interrupt
                        [ Animation.set [ Animation.display Animation.flex ]
                        , Animation.set [ Animation.opacity 0.8 ]
                        , Animation.wait (millisToPosix 1000)
                        , Animation.to [ Animation.opacity 0.0 ]
                        , Animation.set [ Animation.display Animation.none ]
                        ]
                        toast.style
            in
            ( { model | toast = { toast | style = newStyle, message = message } }
            , Cmd.none
            )

        AnimateToast animationMsg ->
            let
                toast =
                    model.toast
            in
            ( { model | toast = { toast | style = Animation.update animationMsg model.toast.style } }
            , Cmd.none
            )


view : Model -> Html Msg
view model =
    case model.gamePhase of
        NotStarted ->
            viewIntro model.player model.toast

        ShowingHostOptions ->
            viewHostOptions model.textString model.gameId (getAllPlayers model)

        ConnectingAsGuest ->
            viewGuestLobby model.gamePhase (getAllPlayers model)

        ConnectedAsGuest ->
            viewGuestLobby model.gamePhase (getAllPlayers model)

        InGame poem ->
            viewGame poem model

        GameOver ->
            viewIntro model.player model.toast
