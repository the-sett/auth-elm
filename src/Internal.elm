module Internal exposing (AuthCmd(..), AuthState, Credentials)

import Date exposing (Date)


type alias Credentials =
    { username : String
    , password : String
    }


{-|
Defines the side effects that consumers of the auth module may request.
-}
type AuthCmd
    = Login Credentials
    | Refresh
    | Logout
    | Unauthed


{-| A sub-section of the auth module state describing whether or not the user
is logged in, what permissions they have, and when their auth token will expire.
This is the part of the auth state that most consumers of the Auth module are
interested in.
A set of operators is provided to extract information from the AuthState for
convenience.
-}
type alias AuthState =
    { loggedIn : Bool
    , permissions : List String
    , expiresAt : Maybe Date
    , username : String
    }
