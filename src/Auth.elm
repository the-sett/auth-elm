module Auth exposing
    ( Config, Credentials, Status(..)
    , login, refresh, logout, unauthed
    , Model, Msg, init, update
    )

{-| Manages the state of the authentication process, and provides an API
to request authentication operations.

@docs Config, Credentials, Status
@docs login, refresh, logout, unauthed
@docs Model, Msg, init, update

-}

import Auth.Service
import AuthState exposing (AuthState, Authenticated)
import Http
import Jwt exposing (Token)
import Model
import Process
import Result
import Task
import Time exposing (Posix)
import Utils exposing (message)



-- Time constants


second : Int
second =
    1000



-- The Auth API


{-| The configuration specifying the API root to authenticate against.
-}
type alias Config =
    { authApiRoot : String
    }


{-| Username and password based login credentials.
-}
type alias Credentials =
    { username : String
    , password : String
    }


{-| Auth states of interest to the consumer.
-}
type Status
    = Failed
    | LoggedOut
    | LoggedIn
        { scopes : List String
        , subject : String
        }


{-| Requests that a login be performed.
-}
login : Credentials -> Cmd Msg
login authRequest =
    LogIn authRequest |> message


{-| Requests that an attempt be made to refresh the auth token from the refresh
token.
-}
refresh : Cmd Msg
refresh =
    Refresh |> message


{-| Requests that a logout (including notifying the server of the logout) be
performed.
-}
logout : Cmd Msg
logout =
    LogOut |> message


{-| Requests that the auth state be cleared to the LoggedOut state. Usually in
response to receiving a 401 or 403 error from a server.
-}
unauthed : Cmd Msg
unauthed =
    NotAuthed |> message


{-| The complete state of this auth module. The 'innerModel' is opaque and holds
the private state. The 'state' provides the state as visible to the consumer of
this module.
-}
type alias Model =
    { authApiRoot : String
    , innerModel : Private
    }


{-| Puts an opaque wrapper around the inner model to keep it private (unreadable).
-}
type Private
    = Private AuthState.AuthState


{-| Describes the events this controller responds to.
-}
type Msg
    = LogIn Credentials
    | Refresh
    | LogOut
    | NotAuthed
    | LogInResponse (Result.Result Http.Error Model.AuthResponse)
    | RefreshResponse (Result.Result Http.Error Model.AuthResponse)
    | LogOutResponse (Result.Result Http.Error ())


{-| The initial unauthed state.
-}
init : Config -> Model
init config =
    { authApiRoot = config.authApiRoot
    , innerModel = Private AuthState.loggedOut
    }


{-| Updates the model from Auth commands.
-}
update : Msg -> Model -> ( Model, Cmd Msg, Maybe Status )
update msg model =
    let
        ( innerModel, cmds, maybeStatus ) =
            getAuthState model |> innerUpdate model.authApiRoot msg
    in
    ( setAuthState innerModel model, cmds, maybeStatus )


{-| Lifts the inner model out of the model.
-}
getAuthState : Model -> AuthState.AuthState
getAuthState model =
    let
        (Private inner) =
            model.innerModel
    in
    inner


{-| Lowers the inner model into the model.
-}
setAuthState : AuthState -> Model -> Model
setAuthState inner model =
    { model | innerModel = Private inner }


{-| Extracts a summary view of the authentication status from the model.
-}
getStatus : AuthState -> Status
getStatus authState =
    let
        extract : AuthState.State p { auth : Authenticated } -> { scopes : List String, subject : String }
        extract state =
            let
                authModel =
                    AuthState.untag state
            in
            { scopes = authModel.auth.scopes, subject = authModel.auth.subject }
    in
    case authState of
        AuthState.LoggedOut _ ->
            LoggedOut

        AuthState.Restoring _ ->
            LoggedOut

        AuthState.Attempting _ ->
            LoggedOut

        AuthState.Failed _ ->
            Failed

        AuthState.LoggedIn state ->
            LoggedIn <| extract state

        AuthState.Refreshing state ->
            LoggedIn <| extract state


{-| Compares two AuthStates and outputs the status of the newer one, if it differs
from the older one, otherwise Nothing.
-}
statusChange : AuthState -> AuthState -> Maybe Status
statusChange oldAuthState newAuthState =
    let
        oldStatus =
            getStatus oldAuthState

        newStatus =
            getStatus newAuthState
    in
    case ( oldStatus, newStatus ) of
        ( LoggedIn _, LoggedIn _ ) ->
            Nothing

        ( Failed, Failed ) ->
            Nothing

        ( LoggedOut, LoggedOut ) ->
            Nothing

        ( _, _ ) ->
            Just newStatus


noop authState =
    ( authState, Cmd.none, Nothing )


reset authState =
    let
        newAuthState =
            AuthState.loggedOut
    in
    ( newAuthState, Cmd.none, statusChange authState newAuthState )


failed authState state =
    let
        newAuthState =
            AuthState.toFailed state
    in
    ( newAuthState, Cmd.none, statusChange authState newAuthState )


{-| Updates the auth state and triggers events needed to communicate with the
auth server.
-}
innerUpdate : String -> Msg -> AuthState -> ( AuthState, Cmd Msg, Maybe Status )
innerUpdate authApiRoot msg authState =
    case ( msg, authState ) of
        ( LogIn credentials, AuthState.LoggedOut state ) ->
            ( AuthState.toAttempting state
            , Auth.Service.invokeLogin authApiRoot LogInResponse (authRequestFromCredentials credentials)
            , Nothing
            )

        ( Refresh, AuthState.LoggedOut state ) ->
            ( AuthState.toRestoring state
            , Auth.Service.invokeRefresh authApiRoot RefreshResponse
            , Nothing
            )

        ( Refresh, AuthState.LoggedIn state ) ->
            ( AuthState.toRefreshing state
            , Auth.Service.invokeRefresh authApiRoot RefreshResponse
            , Nothing
            )

        ( LogOut, _ ) ->
            ( authState
            , Auth.Service.invokeLogout authApiRoot LogOutResponse
            , Nothing
            )

        ( NotAuthed, _ ) ->
            reset authState

        ( LogInResponse result, AuthState.Attempting state ) ->
            case result of
                Err _ ->
                    failed authState state

                Ok (Model.AuthResponse response) ->
                    case Jwt.decode response.token of
                        Nothing ->
                            noop authState

                        Just decodedToken ->
                            toLoggedInFromToken authApiRoot response.token decodedToken authState state

        ( RefreshResponse result, AuthState.Restoring state ) ->
            case result of
                Err _ ->
                    ( AuthState.loggedOut, Cmd.none, Just LoggedOut )

                Ok (Model.AuthResponse response) ->
                    case Jwt.decode response.token of
                        Nothing ->
                            noop authState

                        Just decodedToken ->
                            toLoggedInFromToken authApiRoot response.token decodedToken authState state

        ( RefreshResponse result, AuthState.Refreshing state ) ->
            case result of
                Err _ ->
                    reset authState

                Ok (Model.AuthResponse response) ->
                    case Jwt.decode response.token of
                        Nothing ->
                            noop authState

                        Just decodedToken ->
                            toLoggedInFromToken authApiRoot response.token decodedToken authState state

        ( LogOutResponse result, _ ) ->
            reset authState

        ( _, _ ) ->
            noop authState


toLoggedInFromToken :
    String
    -> String
    -> Token
    -> AuthState
    -> AuthState.State { p | loggedIn : AuthState.Allowed } m
    -> ( AuthState, Cmd Msg, Maybe Status )
toLoggedInFromToken authApiRoot token decodedToken authState state =
    let
        authModel =
            authenticatedFromToken token decodedToken

        newAuthState =
            AuthState.toLoggedInWithAuthenticated authModel state
    in
    ( newAuthState
    , delayedRefreshCmd authModel
    , statusChange authState newAuthState
    )



-- Helper functions for manipuluating the model.


authenticatedFromToken : String -> Token -> Authenticated
authenticatedFromToken rawToken token =
    { token = rawToken
    , decodedToken = token
    , scopes = token.scopes
    , expiresAt = token.exp
    , subject = token.sub
    , refreshFrom = token.exp
    }


authRequestFromCredentials : Credentials -> Model.AuthRequest
authRequestFromCredentials credentials =
    Model.AuthRequest
        { username = credentials.username
        , password = credentials.password
        }



-- Functions for building and executing the refresh cycle task.


delayedRefreshCmd : Authenticated -> Cmd Msg
delayedRefreshCmd model =
    tokenExpiryTask model.refreshFrom
        |> Task.attempt (\_ -> Refresh)


{-| A delay task that should end 30 seconds before the token is due to expire.
If the token expiry is less than 1 minute away, the delay is set to half of the remaining
time, which should be under 30 seconds.
The delay will expire immediately if the token expiry is already in the past.
-}
tokenExpiryTask : Posix -> Task.Task Never ()
tokenExpiryTask timeout =
    let
        safeInterval =
            30 * second

        delay posixBy posixNow =
            let
                by =
                    Time.posixToMillis posixBy

                now =
                    Time.posixToMillis posixNow
            in
            max ((by - now) // 2) (by - now - safeInterval)
                |> max 0
    in
    Time.now
        |> Task.andThen (\now -> Process.sleep <| toFloat (delay timeout now))
