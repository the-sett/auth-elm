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
import AuthState exposing (AuthenticatedModel)


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
    { innerModel : Private
    , state : AuthenticationState
    }


{-| Puts an opaque wrapper around the inner model to keep it private (unreadable).
-}
type Private
    = Private InnerModel


type alias InnerModel =
    { authApiRoot : String
    , authState : AuthState.AuthState
    }


{-| Lifts the inner model out of the model.
-}
liftModel : Model -> InnerModel
liftModel model =
    let
        (Private inner) =
            model.innerModel
    in
        inner


{-| Lowers the inner model into the model.
-}
lowerModel : InnerModel -> Model
lowerModel inner =
    let
        extract : AuthState.State p { auth : AuthenticatedModel } -> { permissions : List String, username : String }
        extract state =
            let
                authModel =
                    AuthState.untag state
            in
                { permissions = authModel.auth.permissions, username = authModel.auth.username }
    in
        case inner.authState of
            AuthState.LoggedOut _ ->
                { innerModel = Private inner, state = LoggedOut }

            AuthState.Restoring _ ->
                { innerModel = Private inner, state = LoggedOut }

            AuthState.Attempting _ ->
                { innerModel = Private inner, state = LoggedOut }

            AuthState.Failed _ ->
                { innerModel = Private inner, state = Failed }

            AuthState.LoggedIn state ->
                { innerModel = Private inner, state = LoggedIn <| extract state }

            AuthState.Refreshing state ->
                { innerModel = Private inner, state = LoggedIn <| extract state }


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
    lowerModel
        { authApiRoot = config.authApiRoot
        , authState = AuthState.loggedOut
        }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        ( innerModel, cmds ) =
            liftModel model |> innerUpdate msg
    in
        ( lowerModel innerModel, cmds )


{-| Updates the auth state and triggers events needed to communicate with the
auth server.
-}
innerUpdate : Msg -> InnerModel -> ( InnerModel, Cmd Msg )
innerUpdate msg model =
    -- case msg of
    --     AuthApi action_ ->
    --         Auth.Service.update callbacks action_ (Model model)
    --
    --     LogIn credentials ->
    --         ( Model
    --             { model
    --                 | token = Nothing
    --                 , authState = authStateFromToken Nothing
    --                 , logonAttempted = True
    --             }
    --         , Auth.Service.invokeLogin model.authApiRoot AuthApi (authRequestFromCredentials credentials)
    --         )
    --
    --     Refresh ->
    --         ( Model { model | logonAttempted = False }, Auth.Service.invokeRefresh model.authApiRoot AuthApi )
    --
    --     LogOut ->
    --         ( Model { model | logonAttempted = False }, Auth.Service.invokeLogout model.authApiRoot AuthApi )
    --
    --     NotAuthed ->
    --         ( Model
    --             { model
    --                 | token = Nothing
    --                 , authState = authStateFromToken Nothing
    --                 , logonAttempted = False
    --             }
    --         , Cmd.none
    --         )
    --
    --     Refreshed result ->
    --         case result of
    --             Err _ ->
    --                 ( Model model, Cmd.none )
    --
    --             Ok authResponse ->
    --                 if isLoggedIn (Model model) then
    --                     refreshResponse authResponse (Model model)
    --                 else
    --                     ( Model model, Cmd.none )
    let
        noop =
            ( model, Cmd.none )
    in
        case ( model.authState, msg ) of
            ( _, AuthApi apiMsg ) ->
                Auth.Service.update callbacks apiMsg model

            _ ->
                noop



{--Helper functions over the auth model. --}


refreshTimeFromToken : Maybe Token -> Maybe Date
refreshTimeFromToken maybeToken =
    let
        maybeDate =
            Maybe.map (\token -> token.exp) maybeToken
    in
        Maybe.map (\date -> (Date.toTime date) - 30 * Time.second |> Date.fromTime) maybeDate



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


callbacks : Auth.Service.Callbacks InnerModel Msg
callbacks =
    { login = loginResponse
    , refresh = refreshResponse
    , logout = logoutResponse
    , error = \_ -> \model -> ( model, Cmd.none )
    }


loginResponse : Model.AuthResponse -> InnerModel -> ( InnerModel, Cmd Msg )
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


refreshResponse : Model.AuthResponse -> InnerModel -> ( InnerModel, Cmd Msg )
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


logoutResponse : InnerModel -> ( InnerModel, Cmd Msg )
logoutResponse model =
    -- ( Model { model | token = Nothing, authState = authStateFromToken Nothing }
    -- , Cmd.none
    -- )
    ( model, Cmd.none )



-- Functions for building and executing the refresh cycle task.


delayedRefreshCmd : InnerModel -> Cmd Msg
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
