module AuthState
    exposing
        ( AuthState(..)
        , AuthenticatedModel
          -- Accessing the inner model
        , State
        , untag
          -- Constructors
        , loggedOut
          -- Map
        , mapAuthenticatedModel
          -- State transitions
        , toRestoring
        , toAttempting
        , toFailed
        , toLoggedInWithAuthenticatedModel
        , toRefreshing
        )

import Date exposing (Date)
import Jwt exposing (Token)
import StateMachine exposing (State(..), Allowed, map)


untag : State tag value -> value
untag =
    StateMachine.untag


type alias State p m =
    StateMachine.State p m


type alias AuthenticatedModel =
    { token : String
    , decodedToken : Token
    , permissions : List String
    , expiresAt : Date
    , refreshFrom : Date
    , username : String
    }


{-| Note that the LoggedOut state is effectively a reset on the state machine,
and is allowed from any state, so it is not marked explcitly here.
-}
type AuthState
    = LoggedOut (State { restoring : Allowed, attempting : Allowed } {})
    | Restoring (State { loggedIn : Allowed } {})
    | Attempting (State { loggedIn : Allowed, failed : Allowed } {})
    | Failed (State {} {})
    | LoggedIn (State { refreshing : Allowed } { auth : AuthenticatedModel })
    | Refreshing (State { loggedIn : Allowed } { auth : AuthenticatedModel })



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
    State { auth = model } |> LoggedIn


refreshing : AuthenticatedModel -> AuthState
refreshing model =
    State { auth = model } |> Refreshing



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
