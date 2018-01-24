module TopState
    exposing
        ( Session(..)
        , WithWelcome
        , WithContentEditor
        , initial
        , updateWelcome
        , updateContentEditor
        , toWelcome
        , toWelcomeWithWelcome
        , toFailedAuth
        , toAuthenticatedWithContentEditor
        , untag
        )

import Login
import StateMachine exposing (State(..), Allowed)


type alias WithWelcome =
    { welcome : Welcome.Auth.Model }


type alias WithContentEditor =
    { contentEditor : CE.Model }


type Session
    = Initial (State { welcome : Allowed } {})
    | Welcome (State { authenticated : Allowed, failedAuth : Allowed } WithWelcome)
    | FailedAuth (State { welcome : Allowed } WithWelcome)
    | Authenticated (State { welcome : Allowed } WithContentEditor)


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


authenticated : WithContentEditor -> Session
authenticated editor =
    State editor |> Authenticated



-- Update functions that can be applied when parts of the model are present.


updateWelcome :
    (WithWelcome -> WithWelcome)
    -> State p WithWelcome
    -> State p WithWelcome
updateWelcome func state =
    StateMachine.map func state


updateContentEditor :
    (WithContentEditor -> WithContentEditor)
    -> State p WithContentEditor
    -> State p WithContentEditor
updateContentEditor func state =
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


toAuthenticatedWithContentEditor : WithContentEditor -> State { a | authenticated : Allowed } m -> Session
toAuthenticatedWithContentEditor contentEditor _ =
    authenticated contentEditor
