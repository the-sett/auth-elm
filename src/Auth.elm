module Auth
    exposing
        ( Model
        , Msg
        , init
        , logonAttempted
        , update
        , Credentials
        , isLoggedIn
        , permissions
        , expiresAt
        , username
        , hasPermission
        , login
        , refresh
        , logout
        , unauthed
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
import AuthState


{-| The complete state of this auth module.
-}
type Model
    = Model
        { token : Maybe String
        , decodedToken : Maybe Token
        , refreshFrom : Maybe Date
        , errorMsg : String
        , authState : AuthState
        , authApiRoot : String
        , logonAttempted : Bool
        }


{-| A sub-section of the auth module state describing whether or not the user
is logged in, what permissions they have, and when their auth token will expire.
This is the part of the auth state that most consumers of the Auth module are
interested in.
-}
type alias AuthState =
    { loggedIn : Bool
    , permissions : List String
    , expiresAt : Date
    , username : String
    }


type alias Config =
    { authApiRoot : String
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
    model.authState.loggedIn


{-| Obtains a list of permissions held by the current user.
-}
permissions : Model -> List String
permissions (Model model) =
    []


{-| Checks when the current users token will expire. The user may not have a token,
or may not have one that expires at all, in which case Nothing will be returned.
-}
expiresAt : Model -> Maybe Date
expiresAt (Model model) =
    Nothing


{-| Obtains the current users username, provided they are logged in. If the user
is not logged in, the empty string will be returned.
-}
username : Model -> String
username (Model model) =
    ""


{-| Checks if the current user has a particular named permission.
-}
hasPermission : String -> Model -> Bool
hasPermission permission (Model model) =
    List.member permission model.authState.permissions


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


{-| Requests that the auth state be cleared to the unauthed state. Usually in
response to receiving a 401 or 403 error from a server.
-}
unauthed : Msg
unauthed =
    NotAuthed


{-| The initial unauthed state.
-}
init : Config -> Model
init config =
    Model
        { token = Nothing
        , decodedToken = Nothing
        , refreshFrom = Nothing
        , errorMsg = ""
        , authState = notAuthedState
        , authApiRoot = config.authApiRoot
        , logonAttempted = False
        }



{--Helper functions over the auth model. --}


notAuthedState =
    { loggedIn = False
    , permissions = []
    , expiresAt = Date.fromTime 0
    , username = ""
    }


credentialsDecoder : Decoder Credentials
credentialsDecoder =
    (Decode.succeed
        (\username password ->
            { username = username
            , password = password
            }
        )
    )
        |: (Decode.field "username" Decode.string)
        |: (Decode.field "password" Decode.string)


decodeCredentials : Encode.Value -> Maybe Credentials
decodeCredentials val =
    Decode.decodeValue credentialsDecoder val |> Result.toMaybe


refreshTimeFromToken : Maybe Token -> Maybe Date
refreshTimeFromToken maybeToken =
    let
        maybeDate =
            Maybe.map (\token -> token.exp) maybeToken
    in
        Maybe.map (\date -> (Date.toTime date) - 30 * Time.second |> Date.fromTime) maybeDate


authStateFromToken : Maybe Token -> AuthState
authStateFromToken maybeToken =
    case maybeToken of
        Nothing ->
            notAuthedState

        Just token ->
            { loggedIn = True
            , permissions = token.scopes
            , expiresAt = token.exp
            , username = token.sub
            }


{-| Reports whether a logon has been attempted.
-}
logonAttempted : Model -> Bool
logonAttempted (Model model) =
    model.logonAttempted



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
    let
        decodedToken =
            Maybe.map Jwt.decode response.token
                |> Maybe.Extra.join

        model_ =
            Model
                { model
                    | token = response.token
                    , refreshFrom = refreshTimeFromToken decodedToken
                    , decodedToken = decodedToken
                    , authState = authStateFromToken decodedToken
                }
    in
        ( model_
        , Cmd.batch
            [ delayedRefreshCmd model_
            ]
        )


refreshResponse : Model.AuthResponse -> Model -> ( Model, Cmd Msg )
refreshResponse (Model.AuthResponse response) (Model model) =
    let
        decodedToken =
            Maybe.map Jwt.decode response.token
                |> Maybe.Extra.join

        model_ =
            Model
                { model
                    | token = response.token
                    , refreshFrom = refreshTimeFromToken decodedToken
                    , decodedToken = decodedToken
                    , authState = authStateFromToken decodedToken
                }
    in
        ( model_, delayedRefreshCmd model_ )


logoutResponse : Model -> ( Model, Cmd Msg )
logoutResponse (Model model) =
    ( Model { model | token = Nothing, authState = authStateFromToken Nothing }
    , Cmd.none
    )



-- Functions for building and executing the refresh cycle task.


delayedRefreshCmd : Model -> Cmd Msg
delayedRefreshCmd (Model model) =
    case model.refreshFrom of
        Nothing ->
            Cmd.none

        Just refreshDate ->
            tokenExpiryTask model.authApiRoot refreshDate
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
        { username = Just credentials.username
        , password = Just credentials.password
        }



-- Event handler.


{-| Updates the auth state and triggers events needed to communicate with the
auth server.
-}
update : Msg -> Model -> ( Model, Cmd Msg )
update msg (Model model) =
    case msg of
        AuthApi action_ ->
            Auth.Service.update callbacks action_ (Model model)

        LogIn credentials ->
            ( Model
                { model
                    | token = Nothing
                    , authState = authStateFromToken Nothing
                    , logonAttempted = True
                }
            , Auth.Service.invokeLogin model.authApiRoot AuthApi (authRequestFromCredentials credentials)
            )

        Refresh ->
            ( Model { model | logonAttempted = False }, Auth.Service.invokeRefresh model.authApiRoot AuthApi )

        LogOut ->
            ( Model { model | logonAttempted = False }, Auth.Service.invokeLogout model.authApiRoot AuthApi )

        NotAuthed ->
            ( Model
                { model
                    | token = Nothing
                    , authState = authStateFromToken Nothing
                    , logonAttempted = False
                }
            , Cmd.none
            )

        Refreshed result ->
            case result of
                Err _ ->
                    ( Model model, Cmd.none )

                Ok authResponse ->
                    if isLoggedIn (Model model) then
                        refreshResponse authResponse (Model model)
                    else
                        ( Model model, Cmd.none )
