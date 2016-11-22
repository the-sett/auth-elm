port module Auth
    exposing
        ( login
        , refresh
        , logout
        , unauthed
        , Credentials
        )

{-|
Provides noitification commands to request changes to the auth state for login,
logout, refresh and unauthed state changes.
@docs login, logout, refresh, unauthed, Credentials
-}

import Elmq
import Json.Encode as Encode


{-|
Username and password based login credentials.
-}
type alias Credentials =
    { username : String
    , password : String
    }


credentialsEncoder : Credentials -> Encode.Value
credentialsEncoder model =
    [ ( "username", Encode.string model.username )
    , ( "password", Encode.string model.password )
    ]
        |> Encode.object


{-|
Requests that a login be performed.
-}
login : Credentials -> Cmd msg
login authRequest =
    Elmq.send "auth.login" <| credentialsEncoder authRequest


{-|
Requests that an attempt be made to refresh the auth token from the refresh
token.
-}
refresh : Cmd msg
refresh =
    Elmq.sendNaked "auth.refresh"


{-|
Requests that a logout (including notifying the server of the logout) be
performed.
-}
logout : Cmd msg
logout =
    Elmq.sendNaked "auth.logout"


{-|
Requests that the auth state be cleared to the unauthed state. Usually in
response to receiving a 401 or 403 error from a server.
-}
unauthed : Cmd msg
unauthed =
    Elmq.sendNaked "auth.unauthed"
