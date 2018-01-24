module TopState
    exposing
        ( Session(..)
        , initial
        , updateWelcome
        , toWelcome
        , toWelcomeWithLoginModel
        , toFailedAuth
        , toAuthenticated
        , untag
        )

import Login
import StateMachine exposing (State(..), Allowed)


type Session
    = Initial (State { loginState : Allowed } {})
    | Welcome (State { authenticated : Allowed, failedAuth : Allowed } Login.Model)
    | FailedAuth (State { loginState : Allowed } Login.Model)
    | Authenticated (State { loginState : Allowed } {})


untag : State tag value -> value
untag =
    StateMachine.untag



-- State constructors.


initial : Session
initial =
    State {} |> Initial


welcome : Login.Model -> Session
welcome loginState =
    State loginState |> Welcome


failedAuth : Login.Model -> Session
failedAuth loginState =
    State loginState |> FailedAuth


authenticated : Session
authenticated =
    State {} |> Authenticated



-- Update functions that can be applied when parts of the model are present.


updateWelcome :
    (Login.Model -> Login.Model)
    -> State p Login.Model
    -> State p Login.Model
updateWelcome func state =
    StateMachine.map func state



-- State transition functions that can be applied only to states that are permitted
-- to make a transition.


toWelcome : State { a | loginState : Allowed } Login.Model -> Session
toWelcome (State model) =
    welcome model


toWelcomeWithLoginModel : Login.Model -> State { a | loginState : Allowed } m -> Session
toWelcomeWithLoginModel loginState _ =
    welcome loginState


toFailedAuth : State { a | failedAuth : Allowed } Login.Model -> Session
toFailedAuth (State model) =
    failedAuth model


toAuthenticated : State { a | authenticated : Allowed } Login.Model -> Session
toAuthenticated _ =
    authenticated
