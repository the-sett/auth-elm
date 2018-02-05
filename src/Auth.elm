module Auth
    exposing
        ( Config
        , Credentials
        , Status(..)
        , login
        , refresh
        , logout
        , unauthed
        , getStatus
        , Model
        , Msg
        , init
        , update
        )

{-| Maintains the auth state and follows the TEA pattern to provide a stateful auth
module that can be wired in to TEA applications update cycles.
@docs Model, Msg
@docs init, logonAttempted, update, updateFromMsg, getStatus
-}

import Date exposing (Date)
import Time
import Maybe.Extra
import Task exposing (andThen)
import Process
import Navigation
import Http
import Result
import Json.Encode as Encode
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Extra exposing ((|:), withDefault)
import Jwt exposing (Token)
import Auth.Service
import Model
import AuthState exposing (AuthState, Authenticated)
import Utils exposing (message)


-- The Auth API


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


getStatus : Model -> Status
getStatus model =
    let
        extract : AuthState.State p { auth : Authenticated } -> { scopes : List String, subject : String }
        extract state =
            let
                authModel =
                    AuthState.untag state
            in
                { scopes = authModel.auth.scopes, subject = authModel.auth.subject }

        (Private inner) =
            model.innerModel
    in
        case inner of
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


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        ( innerModel, cmds ) =
            getAuthState model |> innerUpdate model.authApiRoot msg
    in
        ( setAuthState innerModel model, cmds )


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


noop authState =
    ( authState, Cmd.none )


reset =
    ( AuthState.loggedOut, Cmd.none )


failed state =
    ( AuthState.toFailed state, Cmd.none )


{-| Updates the auth state and triggers events needed to communicate with the
auth server.
-}
innerUpdate : String -> Msg -> AuthState -> ( AuthState, Cmd Msg )
innerUpdate authApiRoot msg authState =
    case ( Debug.log "auth" msg, authState ) of
        ( LogIn credentials, AuthState.LoggedOut state ) ->
            ( AuthState.toAttempting state
            , Auth.Service.invokeLogin authApiRoot LogInResponse (authRequestFromCredentials credentials)
            )

        ( Refresh, AuthState.LoggedIn state ) ->
            ( AuthState.toRefreshing state
            , Auth.Service.invokeRefresh authApiRoot RefreshResponse
            )

        ( LogOut, _ ) ->
            ( authState, Auth.Service.invokeLogout authApiRoot LogOutResponse )

        ( NotAuthed, _ ) ->
            reset

        ( LogInResponse result, AuthState.Attempting state ) ->
            case result of
                Err _ ->
                    failed state

                Ok (Model.AuthResponse response) ->
                    case Jwt.decode response.token of
                        Nothing ->
                            noop authState

                        Just decodedToken ->
                            toLoggedInFromToken authApiRoot response.token decodedToken state

        ( RefreshResponse result, AuthState.Refreshing state ) ->
            case result of
                Err _ ->
                    reset

                Ok (Model.AuthResponse response) ->
                    case Jwt.decode response.token of
                        Nothing ->
                            noop authState

                        Just decodedToken ->
                            toLoggedInFromToken authApiRoot response.token decodedToken state

        ( LogOutResponse result, _ ) ->
            reset

        ( _, _ ) ->
            noop authState


toLoggedInFromToken :
    String
    -> String
    -> Token
    -> AuthState.State { p | loggedIn : AuthState.Allowed } m
    -> ( AuthState, Cmd Msg )
toLoggedInFromToken authApiRoot token decodedToken state =
    let
        authModel =
            authenticatedFromToken token decodedToken
    in
        ( AuthState.toLoggedInWithAuthenticated authModel state
        , delayedRefreshCmd authApiRoot authModel
        )



-- Helper functions for manipluating the model.


refreshTimeFromToken : Token -> Date
refreshTimeFromToken token =
    (Date.toTime token.exp) - 30 * Time.second |> Date.fromTime


authenticatedFromToken : String -> Token -> Authenticated
authenticatedFromToken rawToken token =
    { token = rawToken
    , decodedToken = token
    , scopes = token.scopes
    , expiresAt = token.exp
    , subject = token.sub
    , refreshFrom = refreshTimeFromToken token
    }


authRequestFromCredentials : Credentials -> Model.AuthRequest
authRequestFromCredentials credentials =
    Model.AuthRequest
        { username = credentials.username
        , password = credentials.password
        }



-- Functions for building and executing the refresh cycle task.


delayedRefreshCmd : String -> Authenticated -> Cmd Msg
delayedRefreshCmd authApiRoot model =
    tokenExpiryTask authApiRoot model.refreshFrom
        |> Task.attempt (\result -> RefreshResponse result)


tokenExpiryTask : String -> Date -> Task.Task Http.Error Model.AuthResponse
tokenExpiryTask root refreshDate =
    let
        delay refreshDate now =
            max 0 ((Date.toTime refreshDate) - now)
    in
        Time.now
            |> andThen (\now -> Process.sleep <| delay refreshDate now)
            |> andThen (\_ -> Auth.Service.refreshTask root |> Http.toTask)
