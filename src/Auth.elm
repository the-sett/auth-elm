module Auth
    exposing
        ( Config
        , Credentials
        , login
        , refresh
        , logout
        , unauthed
        , Model
        , Msg
        , AuthenticationState(..)
        , init
        , update
        )

{-| Maintains the auth state and follows the TEA pattern to provide a stateful auth
module that can be wired in to TEA applications update cycles.
@docs Model, Msg
@docs init, logonAttempted, update, updateFromMsg, extractAuthState, updateForwardLocation
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
import AuthState exposing (AuthState, AuthenticatedModel)


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


{-| Requests that a login be performed.
-}
login : Credentials -> Msg
login authRequest =
    LogIn authRequest


{-| Requests that an attempt be made to refresh the auth token from the refresh
token.
-}
refresh : Msg
refresh =
    Refresh


{-| Requests that a logout (including notifying the server of the logout) be
performed.
-}
logout : Msg
logout =
    LogOut


{-| Requests that the auth state be cleared to the LoggedOut state. Usually in
response to receiving a 401 or 403 error from a server.
-}
unauthed : Msg
unauthed =
    NotAuthed


{-| Auth states of interest to the consumer.
-}
type AuthenticationState
    = Failed
    | LoggedOut
    | LoggedIn
        { permissions : List String
        , username : String
        }



-- TEA encapsulation of this module.


{-| The complete state of this auth module. The 'innerModel' is opaque and holds
the private state. The 'state' provides the state as visible to the consumer of
this module.
-}
type alias Model =
    { authApiRoot : String
    , innerModel : Private
    , state : AuthenticationState
    }


{-| Puts an opaque wrapper around the inner model to keep it private (unreadable).
-}
type Private
    = Private AuthState.AuthState


{-| Lifts the inner model out of the model.
-}
liftModel : Model -> AuthState.AuthState
liftModel model =
    let
        (Private inner) =
            model.innerModel
    in
        inner


{-| Lowers the inner model into the model.
-}
lowerModel : AuthState -> Model -> Model
lowerModel inner model =
    let
        extract : AuthState.State p { auth : AuthenticatedModel } -> { permissions : List String, username : String }
        extract state =
            let
                authModel =
                    AuthState.untag state
            in
                { permissions = authModel.auth.permissions, username = authModel.auth.username }
    in
        case inner of
            AuthState.LoggedOut _ ->
                { model | innerModel = Private inner, state = LoggedOut }

            AuthState.Restoring _ ->
                { model | innerModel = Private inner, state = LoggedOut }

            AuthState.Attempting _ ->
                { model | innerModel = Private inner, state = LoggedOut }

            AuthState.Failed _ ->
                { model | innerModel = Private inner, state = Failed }

            AuthState.LoggedIn state ->
                { model | innerModel = Private inner, state = LoggedIn <| extract state }

            AuthState.Refreshing state ->
                { model | innerModel = Private inner, state = LoggedIn <| extract state }


{-| Describes the events this controller responds to.
-}
type Msg
    = LogIn Credentials
    | Refresh
    | LogOut
    | NotAuthed
    | Refreshed (Result.Result Http.Error Model.AuthResponse)
    | LogInResponse (Result.Result Http.Error Model.AuthResponse)
    | RefreshResponse (Result.Result Http.Error Model.AuthResponse)
    | LogOutResponse (Result.Result Http.Error ())


{-| The initial unauthed state.
-}
init : Config -> Model
init config =
    { authApiRoot = config.authApiRoot
    , innerModel = Private AuthState.loggedOut
    , state = LoggedOut
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        ( innerModel, cmds ) =
            liftModel model |> innerUpdate model.authApiRoot msg
    in
        ( lowerModel innerModel model, cmds )


noop authState =
    ( authState, Cmd.none )


reset =
    ( AuthState.loggedOut, Cmd.none )


{-| Updates the auth state and triggers events needed to communicate with the
auth server.
-}
innerUpdate : String -> Msg -> AuthState -> ( AuthState, Cmd Msg )
innerUpdate authApiRoot msg authState =
    case (Debug.log "auth" msg) of
        LogIn credentials ->
            case authState of
                AuthState.LoggedOut state ->
                    ( AuthState.toAttempting state
                    , Auth.Service.invokeLogin authApiRoot LogInResponse (authRequestFromCredentials credentials)
                    )

                _ ->
                    noop authState

        Refresh ->
            case authState of
                AuthState.LoggedIn state ->
                    ( AuthState.toRefreshing state
                    , Auth.Service.invokeRefresh authApiRoot RefreshResponse
                    )

                _ ->
                    noop authState

        LogOut ->
            ( authState, Auth.Service.invokeLogout authApiRoot LogOutResponse )

        NotAuthed ->
            reset

        Refreshed result ->
            case result of
                Err _ ->
                    reset

                Ok authResponse ->
                    (refreshResponse authApiRoot) authResponse authState

        LogInResponse result ->
            case result of
                Err _ ->
                    reset

                Ok authResponse ->
                    logInResponse authApiRoot authResponse authState

        RefreshResponse result ->
            case result of
                Err _ ->
                    reset

                Ok authResponse ->
                    refreshResponse authApiRoot authResponse authState

        LogOutResponse result ->
            reset



{- Helper functions over the auth model. -}


refreshTimeFromToken : Token -> Date
refreshTimeFromToken token =
    (Date.toTime token.exp) - 30 * Time.second |> Date.fromTime


authModelFromToken : String -> Token -> AuthenticatedModel
authModelFromToken rawToken token =
    { token = rawToken
    , decodedToken = token
    , permissions = token.scopes
    , expiresAt = token.exp
    , username = token.sub
    , refreshFrom = refreshTimeFromToken token
    }



-- Auth REST API calls.


logInResponse : String -> Model.AuthResponse -> AuthState -> ( AuthState, Cmd Msg )
logInResponse authApiRoot (Model.AuthResponse response) authState =
    case authState of
        AuthState.Attempting state ->
            let
                maybeToken =
                    Jwt.decode response.token
            in
                case maybeToken of
                    Nothing ->
                        noop authState

                    Just decodedToken ->
                        let
                            authModel =
                                authModelFromToken response.token decodedToken
                        in
                            ( AuthState.toLoggedInWithAuthenticatedModel authModel state
                            , delayedRefreshCmd authApiRoot authModel
                            )

        _ ->
            noop authState


refreshResponse : String -> Model.AuthResponse -> AuthState -> ( AuthState, Cmd Msg )
refreshResponse authApiRoot (Model.AuthResponse response) authState =
    case authState of
        AuthState.Refreshing state ->
            let
                maybeToken =
                    Jwt.decode response.token
            in
                case maybeToken of
                    Nothing ->
                        noop authState

                    Just decodedToken ->
                        let
                            authModel =
                                authModelFromToken response.token decodedToken
                        in
                            ( AuthState.toLoggedInWithAuthenticatedModel authModel state
                            , delayedRefreshCmd authApiRoot authModel
                            )

        _ ->
            noop authState



-- Functions for building and executing the refresh cycle task.


delayedRefreshCmd : String -> AuthenticatedModel -> Cmd Msg
delayedRefreshCmd authApiRoot model =
    tokenExpiryTask authApiRoot model.refreshFrom
        |> Task.attempt (\result -> Refreshed result)


tokenExpiryTask : String -> Date -> Task.Task Http.Error Model.AuthResponse
tokenExpiryTask root refreshDate =
    let
        delay refreshDate now =
            max 0 ((Date.toTime refreshDate) - now)
    in
        Time.now
            |> andThen (\now -> Process.sleep <| delay refreshDate now)
            |> andThen (\_ -> Auth.Service.refreshTask root |> Http.toTask)


authRequestFromCredentials : Credentials -> Model.AuthRequest
authRequestFromCredentials credentials =
    Model.AuthRequest
        { username = credentials.username
        , password = credentials.password
        }
