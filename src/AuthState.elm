module AuthState exposing
    ( Allowed
    , AuthState(..)
    ,  Authenticated
       -- Convenience re-exports from StateMachine

    , State
    ,  loggedOut
       -- Map

    ,  mapAuthenticated
       -- State transitions

    , toAttempting
    , toFailed
    , toLoggedInWithAuthenticated
    , toRefreshing
    , toRestoring
    ,  untag
       -- Constructors

    )

import Jwt exposing (Token)
import StateMachine exposing (Allowed, State(..), map)
import Time exposing (Posix)


untag : State tag value -> value
untag =
    StateMachine.untag


type alias State p m =
    StateMachine.State p m


type alias Allowed =
    StateMachine.Allowed


type alias Authenticated =
    { subject : String
    , scopes : List String
    , token : String
    , decodedToken : Token
    , expiresAt : Posix
    , refreshFrom : Posix
    }


{-| Note that the LoggedOut state is effectively a reset on the state machine,
and is allowed from any state, so it is not marked explcitly here.
-}
type AuthState
    = LoggedOut (State { restoring : Allowed, attempting : Allowed } {})
    | Restoring (State { loggedIn : Allowed } {})
    | Attempting (State { loggedIn : Allowed, failed : Allowed } {})
    | Failed (State {} {})
    | LoggedIn (State { refreshing : Allowed } { auth : Authenticated })
    | Refreshing (State { loggedIn : Allowed } { auth : Authenticated })



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


loggedIn : Authenticated -> AuthState
loggedIn model =
    State { auth = model } |> LoggedIn


refreshing : Authenticated -> AuthState
refreshing model =
    State { auth = model } |> Refreshing



-- Map functions


mapAuth : (a -> a) -> ({ m | auth : a } -> { m | auth : a })
mapAuth func =
    \model -> { model | auth = func model.auth }


mapAuthenticated :
    (Authenticated -> Authenticated)
    -> State p { m | auth : Authenticated }
    -> State p { m | auth : Authenticated }
mapAuthenticated func state =
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


toLoggedInWithAuthenticated : Authenticated -> State { a | loggedIn : Allowed } m -> AuthState
toLoggedInWithAuthenticated authModel _ =
    loggedIn authModel


toRefreshing : State { a | refreshing : Allowed } { m | auth : Authenticated } -> AuthState
toRefreshing (State model) =
    refreshing model.auth
