port module Logger exposing (log)


type alias LogMessage
    = String


{-| Logs a message to somewhere, probably Browser Console
-}
port log : LogMessage -> Cmd msg
