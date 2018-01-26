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
        , AuthenticationState
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
    = AuthApi Auth.Service.Msg
    | LogIn Credentials
    | Refresh
    | LogOut
    | NotAuthed
    | Refreshed (Result.Result Http.Error Model.AuthResponse)


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


{-| Updates the auth state and triggers events needed to communicate with the
auth server.
-}
innerUpdate : String -> Msg -> AuthState -> ( AuthState, Cmd Msg )
innerUpdate authApiRoot msg authState =
    let
        noop =
            ( authState, Cmd.none )
    in
        case msg of
            AuthApi apiMsg ->
                Auth.Service.update callbacks apiMsg authState

            LogIn credentials ->
                case authState of
                    AuthState.LoggedOut state ->
                        ( AuthState.toAttempting state
                        , Auth.Service.invokeLogin authApiRoot AuthApi (authRequestFromCredentials credentials)
                        )

                    _ ->
                        noop

            Refresh ->
                case authState of
                    AuthState.LoggedIn state ->
                        ( AuthState.toRefreshing state
                        , Auth.Service.invokeRefresh authApiRoot AuthApi
                        )

                    _ ->
                        noop

            LogOut ->
                ( authState, Auth.Service.invokeLogout authApiRoot AuthApi )

            NotAuthed ->
                ( AuthState.loggedOut, Cmd.none )

            -- ( _, Refreshed result ) ->
            --     case result of
            --         Err _ ->
            --             ( Model model, Cmd.none )
            --
            --         Ok authResponse ->
            --             if isLoggedIn (Model model) then
            --                 refreshResponse authResponse (Model model)
            --             else
            --                 ( Model model, Cmd.none )
            _ ->
                noop



{--Helper functions over the auth model. --}


refreshTimeFromToken : Token -> Date
refreshTimeFromToken token =
    (Date.toTime token.exp) - 30 * Time.second |> Date.fromTime



-- authStateFromToken : Maybe Token -> AuthState
-- authStateFromToken maybeToken =
--     case maybeToken of
--         Nothing ->
--             notAuthedState
--
--         Just token ->
--             { loggedIn = True
--             , permissions = token.scopes
--             , expiresAt = token.exp
--             , username = token.sub
--             }
-- Auth REST API calls.


callbacks : Auth.Service.Callbacks AuthState Msg
callbacks =
    { login = loginResponse
    , refresh = refreshResponse
    , logout = logoutResponse
    , error = \_ -> \model -> ( model, Cmd.none )
    }


loginResponse : Model.AuthResponse -> AuthState -> ( AuthState, Cmd Msg )
loginResponse (Model.AuthResponse response) model =
    -- let
    --     decodedToken =
    --         Maybe.map Jwt.decode response.token
    --             |> Maybe.Extra.join
    --
    --     model_ =
    --         Model
    --             { model
    --                 | token = response.token
    --                 , refreshFrom = refreshTimeFromToken decodedToken
    --                 , decodedToken = decodedToken
    --                 , authState = authStateFromToken decodedToken
    --             }
    -- in
    --     ( model_
    --     , Cmd.batch
    --         [ delayedRefreshCmd model_
    --         ]
    --     )
    ( model, Cmd.none )


refreshResponse : Model.AuthResponse -> AuthState -> ( AuthState, Cmd Msg )
refreshResponse (Model.AuthResponse response) model =
    -- let
    --     decodedToken =
    --         Maybe.map Jwt.decode response.token
    --             |> Maybe.Extra.join
    --
    --     model_ =
    --         Model
    --             { model
    --                 | token = response.token
    --                 , refreshFrom = refreshTimeFromToken decodedToken
    --                 , decodedToken = decodedToken
    --                 , authState = authStateFromToken decodedToken
    --             }
    -- in
    --     ( model_, delayedRefreshCmd model_ )
    ( model, Cmd.none )


logoutResponse : AuthState -> ( AuthState, Cmd Msg )
logoutResponse model =
    ( AuthState.loggedOut, Cmd.none )



-- ( Model { model | token = Nothing, authState = authStateFromToken Nothing }
-- , Cmd.none
-- )
-- Functions for building and executing the refresh cycle task.


delayedRefreshCmd : AuthState -> Cmd Msg
delayedRefreshCmd model =
    -- case model.refreshFrom of
    --     Nothing ->
    --         Cmd.none
    --
    --     Just refreshDate ->
    --         tokenExpiryTask model.authApiRoot refreshDate
    --             |> Task.attempt (\result -> Refreshed result)
    Cmd.none


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
        { username = Just credentials.username
        , password = Just credentials.password
        }



-- Event handler.
