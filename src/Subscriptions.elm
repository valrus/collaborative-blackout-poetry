module Subscriptions exposing (subscriptions)

import Animation
import Json.Decode as D
import List.Nonempty as NE
import Ports
import State exposing (..)
import Util exposing (IndexRange)



-- SUBSCRIPTIONS


guestJoinedDecoder : D.Decoder GameMessage
guestJoinedDecoder =
    D.map GuestJoined <| D.field "guestName" D.string


playerActionsDecoder : D.Decoder PlayerActions
playerActionsDecoder =
    D.oneOf
        [ D.map RemainingActions D.int
        , D.null Passed
        ]


playerDataDecoder : D.Decoder PlayerData
playerDataDecoder =
    D.map2 PlayerData
        (D.field "name" D.string)
        (D.field "actions" playerActionsDecoder)


playerDecoder : D.Decoder Player
playerDecoder =
    D.field "isHost" D.bool
        |> D.andThen
            (\isHost ->
                if isHost then
                    D.map2 Guest playerDataDecoder (D.succeed Nothing)

                else
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
        (D.field "subtokens" <|
            -- TODO handle multiple subtokens
            D.map NE.singleton
            <|
                D.map2 Subtoken
                    (D.field "range" <|
                        D.map2 IndexRange
                            (D.field "start" D.int)
                            (D.field "end" D.int)
                    )
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
        )



-- D.map NE.singleton <|
--     D.map2 SubToken
--         (D.field "content" D.string)


poemDecoder : D.Decoder Poem
poemDecoder =
    D.array (D.array tokenDecoder)


gameActionDecoder : D.Decoder GameMessage
gameActionDecoder =
    D.map2 GameAction
        (D.at [ "poem" ] poemDecoder)
        (D.at [ "otherPlayers" ] (D.list playerDecoder))


gameEndDecoder : D.Decoder GameMessage
gameEndDecoder =
    D.map GameEnd
        (D.at [ "endPoem" ] poemDecoder)


disconnectionDecoder : D.Decoder GameMessage
disconnectionDecoder =
    D.map Disconnection <| D.field "disconnection" (D.maybe D.string)


gameMessageDecoder : D.Decoder GameMessage
gameMessageDecoder =
    D.oneOf
        [ guestJoinedDecoder -- PlayerName
        , updatePlayerListDecoder -- PlayerList
        , gameActionDecoder -- Poem PlayerList
        , gameEndDecoder -- Poem
        , disconnectionDecoder -- Maybe PlayerName
        ]


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Ports.connectedAsGuest (ConnectedToHost >> GuestMsg)
        , Ports.receivedMessage (ReceivedGameMessage << D.decodeValue gameMessageDecoder)
        , Ports.disconnect (\_ -> ResetToIntro)
        , Animation.subscription AnimateToast [ model.toast.style ]
        ]
