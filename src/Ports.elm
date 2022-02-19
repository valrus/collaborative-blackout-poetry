port module Ports exposing (..)

-- INCOMING


port peerId : (String -> msg) -> Sub msg


port connectionDescription : (String -> msg) -> Sub msg



-- OUTGOING


port startHosting : () -> Cmd msg


port connectToHost : String -> Cmd msg
