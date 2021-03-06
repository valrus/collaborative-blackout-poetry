port module Ports exposing (..)

import Json.Decode as D
import Json.Encode as E



-- INCOMING


port connectedAsGuest : (String -> msg) -> Sub msg


port guestConnected : (String -> msg) -> Sub msg


port receivedMessage : (E.Value -> msg) -> Sub msg


port disconnect : (() -> msg) -> Sub msg



-- OUTGOING


port init : String -> Cmd msg


port startHosting : () -> Cmd msg


port connectToHost : String -> Cmd msg


port sendAsGuest : E.Value -> Cmd msg


port sendAsHost : E.Value -> Cmd msg


port reset :
    Maybe String
    -> Cmd msg -- Tell JS to reset the peer
