module AuthState exposing (..)

import StateMachine exposing (..)


type alias AuthenticatedModel =
    { token : String
    , decodedToken : Token
    , permissions : List String
    , expiresAt : Maybe Date
    , username : String
    }


type AuthState
    = LoggedOut (State { restoring : Allowed, attempting : Allowed } {})
    | Restoring (State { loggedIn : Allowed, loggedOut : Allowed } {})
    | Attempting (State { loggedIn : Allowed, failed : Allowed } {})
    | Failed (State { loggedOut : Allowed } {})
    | LoggedIn (State { loggedOut : Allowed, refreshing : Allowed } { auth : AuthenticatedModel })
    | Refreshing (State { loggedIn : Allowed, loggedOut : Allowed } { auth : AuthenticatedModel })



-- State constructors.


loggedOut : AuthState
loggedOut =
    State {} |> LoggedOut


restoring : AuthState
restoring =
    State {} |> Restoring


attempting : AuthState
attempting =
    State {} |> Attempting


failed : AuthState
failed =
    State {} |> Failed


loggedIn : AuthenticatedModel -> AuthState
loggedIn model =
    State model |> LoggedIn


refreshing : AuthenticatedModel -> AuthState
refreshing model =
    State model |> AuthenticatedModel



-- Map functions


mapAuth : (a -> b) -> ({ m | auth : a } -> { m | auth : b })
mapAuth func =
    \model -> { model | auth = func model.auth }


mapAuthenticatedModel :
    (AuthenticatedModel -> AuthenticatedModel)
    -> State p { m | auth : AuthenticatedModel }
    -> State p { m | auth : AuthenticatedModel }
mapAuthenticatedModel func state =
    map (mapAuth func) state



-- State transition functions that can be applied only to states that are permitted
-- to make a transition.


toLoggedOut : State { a | loggedOut : Allowed } m -> AuthState
toLoggedOut _ =
    loggedOut


toRestoring : State { a | restoring : Allowed } m -> AuthState
toRestoring _ =
    restoring


toAttempting : State { a | attempting : Allowed } m -> AuthState
toAttempting _ =
    attempting


toFailed : State { a | failed : Allowed } m -> AuthState
toFailed _ =
    failed


toLoggedInWithAuthenticatedModel : AuthenticatedModel -> State { a | loggedIn : Allowed } m -> AuthState
toLoggedInWithAuthenticatedModel authModel _ =
    loggedIn authModel


toRefreshing : State { a | refreshing : Allowed } { m | auth : AuthenticatedModel } -> AuthState
toRefreshing (State model) =
    refreshing model.auth
