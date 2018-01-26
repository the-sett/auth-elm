module Auth
    exposing
        ( Config
        , Credentials
        , isLoggedIn
        , permissions
        , username
        , hasPermission
        , login
        , refresh
        , logout
        , unauthed
        , Model
        , Msg
        , AuthEvent
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


{-| Checks if the user is currently authenticated.
-}
isLoggedIn : Model -> Bool
isLoggedIn (Model model) =
    case model.authState of
        AuthState.LoggedIn _ ->
            True

        _ ->
            False


{-| Obtains a list of permissions held by the current user.
-}
permissions : Model -> List String
permissions (Model model) =
    case model.authState of
        AuthState.LoggedIn state ->
            AuthState.untag state |> .auth |> .permissions

        _ ->
            []


{-| Obtains the current users username, provided they are logged in. If the user
is not logged in, the empty string will be returned.
-}
username : Model -> String
username (Model model) =
    case model.authState of
        AuthState.LoggedIn state ->
            AuthState.untag state |> .auth |> .username

        _ ->
            ""


{-| Checks if the current user has a particular named permission.
-}
hasPermission : String -> Model -> Bool
hasPermission permission model =
    List.member permission <| permissions model


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


{-| Auth events of interest to the consumer.
-}
type AuthEvent
    = AuthFailed
    | LoggedOut
    | LoggedIn



-- TEA encapsulation of this module.


{-| The complete state of this auth module.
-}
type Model
    = Model
        { authApiRoot : String
        , authState : AuthState.AuthState
        , event : Maybe AuthEvent
        }


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
    Model
        { authApiRoot = config.authApiRoot
        , authState = AuthState.loggedOut
        , event = Nothing
        }


update : Msg -> Model -> ( Model, Cmd Msg, Maybe AuthEvent )
update msg (Model model) =
    let
        ( Model updatedModel, cmds ) =
            innerUpdate msg (Model model)
    in
        ( Model updatedModel, cmds, updatedModel.event )


{-| Updates the auth state and triggers events needed to communicate with the
auth server.
-}
innerUpdate : Msg -> Model -> ( Model, Cmd Msg )
innerUpdate msg (Model model) =
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
            ( Model { model | event = Nothing }, Cmd.none )
    in
        case ( model.authState, msg ) of
            ( _, AuthApi apiMsg ) ->
                Auth.Service.update callbacks apiMsg (Model model)

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


callbacks : Auth.Service.Callbacks Model Msg
callbacks =
    { login = loginResponse
    , refresh = refreshResponse
    , logout = logoutResponse
    , error = \_ -> \model -> ( model, Cmd.none )
    }


loginResponse : Model.AuthResponse -> Model -> ( Model, Cmd Msg )
loginResponse (Model.AuthResponse response) (Model model) =
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
    ( Model model, Cmd.none )


refreshResponse : Model.AuthResponse -> Model -> ( Model, Cmd Msg )
refreshResponse (Model.AuthResponse response) (Model model) =
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
    ( Model model, Cmd.none )


logoutResponse : Model -> ( Model, Cmd Msg )
logoutResponse (Model model) =
    -- ( Model { model | token = Nothing, authState = authStateFromToken Nothing }
    -- , Cmd.none
    -- )
    ( Model model, Cmd.none )



-- Functions for building and executing the refresh cycle task.


delayedRefreshCmd : Model -> Cmd Msg
delayedRefreshCmd (Model model) =
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
