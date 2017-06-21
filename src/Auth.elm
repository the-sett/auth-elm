module Auth
    exposing
        ( Credentials
        , AuthState
        , isLoggedIn
        , permissions
        , expiresAt
        , username
        , hasPermission
        , AuthCmd
        , login
        , refresh
        , logout
        , unauthed
        )

{-|
Provides the API for interacting with the authentication module, that most
of an application is interested in. This exludes the API that is needed to wire
up the authentication module into the Elm update cycle, which can be found in the
AuthController module.

@docs Credentials
@docs AuthState, isLoggedIn, permissions, expiresAt, username, hasPermission
@docs AuthCmd, login, logout, refresh, unauthed
-}

import Date exposing (Date)
import Internal


{-|
Username and password based login credentials.
-}
type alias Credentials =
    { username : String
    , password : String
    }


{-| A sub-section of the auth module state describing whether or not the user
is logged in, what permissions they have, and when their auth token will expire.
This is the part of the auth state that most consumers of the Auth module are
interested in.
A set of operators is provided to extract information from the AuthState for
convenience.
-}
type alias AuthState =
    Internal.AuthState


{-|
Checks if the user is currently authenticated.
-}
isLoggedIn : AuthState -> Bool
isLoggedIn authState =
    authState.loggedIn


{-|
Obtains a list of permissions held by the current user.
-}
permissions : AuthState -> List String
permissions authState =
    []


{-|
Checks when the current users token will expire. The user may not have a token,
or may not have one that expires at all, in which case Nothing will be returned.
-}
expiresAt : AuthState -> Maybe Date
expiresAt authState =
    Nothing


{-|
Obtains the current users username, provided they are logged in. If the user
is not logged in, the empty string will be returned.
-}
username : AuthState -> String
username authState =
    ""


{-|
Checks if the current user has a particular named permission.
-}
hasPermission : String -> AuthState -> Bool
hasPermission permission authState =
    List.member permission authState.permissions


{-|
Defines the side effects that consumers of the auth module may request.
-}
type alias AuthCmd =
    Internal.AuthCmd


{-|
Requests that a login be performed.
-}
login : Credentials -> AuthCmd
login authRequest =
    Internal.Login authRequest


{-|
Requests that an attempt be made to refresh the auth token from the refresh
token.
-}
refresh : AuthCmd
refresh =
    Internal.Refresh


{-|
Requests that a logout (including notifying the server of the logout) be
performed.
-}
logout : AuthCmd
logout =
    Internal.Logout


{-|
Requests that the auth state be cleared to the unauthed state. Usually in
response to receiving a 401 or 403 error from a server.
-}
unauthed : AuthCmd
unauthed =
    Internal.Unauthed
