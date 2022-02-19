port module Ports exposing (..)

-- INCOMING


port connectedAsGuest : (String -> msg) -> Sub msg



-- OUTGOING


port connectToHost : String -> Cmd msg
