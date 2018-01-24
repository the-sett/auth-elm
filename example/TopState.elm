module TopState
    exposing
        ( Session(..)
        , WithWelcome
        , initial
        , updateWelcome
        , toWelcome
        , toWelcomeWithWelcome
        , toFailedAuth
        , toAuthenticated
        , untag
        )

import Login
import StateMachine exposing (State(..), Allowed)


type alias WithWelcome =
    { welcome : Login.Model }


type Session
    = Initial (State { welcome : Allowed } {})
    | Welcome (State { authenticated : Allowed, failedAuth : Allowed } WithWelcome)
    | FailedAuth (State { welcome : Allowed } WithWelcome)
    | Authenticated (State { welcome : Allowed } {})


untag : State tag value -> value
untag =
    StateMachine.untag



-- State constructors.


initial : Session
initial =
    State {} |> Initial


welcome : WithWelcome -> Session
welcome welcome =
    State welcome |> Welcome


failedAuth : WithWelcome -> Session
failedAuth welcome =
    State welcome |> FailedAuth


authenticated : Session
authenticated =
    State {} |> Authenticated



-- Update functions that can be applied when parts of the model are present.


updateWelcome :
    (WithWelcome -> WithWelcome)
    -> State p WithWelcome
    -> State p WithWelcome
updateWelcome func state =
    StateMachine.map func state



-- State transition functions that can be applied only to states that are permitted
-- to make a transition.


toWelcome : State { a | welcome : Allowed } WithWelcome -> Session
toWelcome (State model) =
    welcome model


toWelcomeWithWelcome : WithWelcome -> State { a | welcome : Allowed } m -> Session
toWelcomeWithWelcome welcomeModel _ =
    welcome welcomeModel


toFailedAuth : State { a | failedAuth : Allowed } WithWelcome -> Session
toFailedAuth (State model) =
    failedAuth model


toAuthenticated : State { a | authenticated : Allowed } m -> Session
toAuthenticated _ =
    authenticated
