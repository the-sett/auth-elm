module AuthState exposing (..)

import StateMachine exposing (..)


type alias InitialModel =
    {}


type alias FailedModel =
    {}


type alias AuthenticatedModel =
    { token : String
    , decodedToken : Token
    , permissions : List String
    , expiresAt : Maybe Date
    , username : String
    }


type Auth
    = LoggedOut (State { restoring : Allowed, attempting : Allowed } InitialModel)
    | Restoring (State { loggedIn : Allowed, loggedOut : Allowed } InitialModel)
    | Attempting (State { loggedIn : Allowed, failed : Allowed } InitialModel)
    | Failed (State { loggedOut : Allowed } FailedModel)
    | LoggedIn (State { loggedOut : Allowed, refreshing : Allowed } AuthenticatedModel)
    | Refreshing (State { loggedIn : Allowed, loggedOut : Allowed } AuthenticatedModel)
