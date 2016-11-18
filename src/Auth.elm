port module Auth exposing (..)


type alias Credentials =
    { username : String
    , password : String
    }


login : Credentials -> Cmd msg
login authRequest =
    sendLogin authRequest


refresh : Cmd msg
refresh =
    sendRefresh ()


logout : Cmd msg
logout =
    sendLogout ()


unauthed : Cmd msg
unauthed =
    sendUnauthed ()


port sendLogin : Credentials -> Cmd msg


port sendLogout : () -> Cmd msg


port sendRefresh : () -> Cmd msg


port sendUnauthed : () -> Cmd msg
