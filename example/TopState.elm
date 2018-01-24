module TopState
    exposing
        ( Session(..)
        , LoginState
        , initial
        , updateWelcome
        , toWelcome
        , toWelcomeWithLoginState
        , toFailedAuth
        , toAuthenticated
        , untag
        )

import Login
import StateMachine exposing (State(..), Allowed)


type alias LoginState =
    { loginState : Login.Model }


type Session
    = Initial (State { loginState : Allowed } {})
    | Welcome (State { authenticated : Allowed, failedAuth : Allowed } LoginState)
    | FailedAuth (State { loginState : Allowed } LoginState)
    | Authenticated (State { loginState : Allowed } {})


untag : State tag value -> value
untag =
    StateMachine.untag



-- State constructors.


initial : Session
initial =
    State {} |> Initial


welcome : LoginState -> Session
welcome loginState =
    State loginState |> Welcome


failedAuth : LoginState -> Session
failedAuth loginState =
    State loginState |> FailedAuth


authenticated : Session
authenticated =
    State {} |> Authenticated



-- Update functions that can be applied when parts of the model are present.


updateWelcome :
    (LoginState -> LoginState)
    -> State p LoginState
    -> State p LoginState
updateWelcome func state =
    StateMachine.map func state



-- State transition functions that can be applied only to states that are permitted
-- to make a transition.


toWelcome : State { a | loginState : Allowed } LoginState -> Session
toWelcome (State model) =
    welcome model


toWelcomeWithLoginState : LoginState -> State { a | loginState : Allowed } m -> Session
toWelcomeWithLoginState loginState _ =
    welcome loginState


toFailedAuth : State { a | failedAuth : Allowed } LoginState -> Session
toFailedAuth (State model) =
    failedAuth model


toAuthenticated : State { a | authenticated : Allowed } LoginState -> Session
toAuthenticated _ =
    authenticated
