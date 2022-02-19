port module Ports exposing (..)

-- INCOMING


port connectionDescription : (String -> msg) -> Sub msg



-- OUTGOING


port startHosting : () -> Cmd msg
