module Auth
    exposing
        ( Model
        , Msg
        , init
        , logonAttempted
        , update
        , updateForwardLocation
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
import Jwt
import Auth.Service
import Model


{-| The complete state of this auth module.
-}
type Model
    = Model
        { token : Maybe String
        , decodedToken : Maybe Token
        , refreshFrom : Maybe Date
        , errorMsg : String
        , authState : AuthState
        , forwardLocation : String
        , logoutLocation : String
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
    , expiresAt : Maybe Date
    , username : String
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


type alias Config =
    { forwardLocation : String
    , logoutLocation : String
    , authApiRoot : String
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
        , forwardLocation = config.forwardLocation
        , logoutLocation = config.logoutLocation
        , authApiRoot = config.authApiRoot
        , logonAttempted = False
        }


{-| Describes the fields of a decoded JWT token.
-}
type alias Token =
    { sub : String
    , iss : Maybe String
    , aud : Maybe String
    , exp : Maybe Date
    , iat : Maybe Date
    , jti : Maybe String
    , scopes : List String
    }



{--Helper functions over the auth model. --}


notAuthedState =
    { loggedIn = False
    , permissions = []
    , expiresAt = Nothing
    , username = ""
    }


tokenDecoder : Decoder Token
tokenDecoder =
    (Decode.succeed
        (\sub iss aud exp iat jti scopes ->
            { sub = sub
            , iss = iss
            , aud = aud
            , exp = exp
            , iat = iat
            , jti = jti
            , scopes = scopes
            }
        )
    )
        |: (Decode.field "sub" Decode.string)
        |: Decode.maybe (Decode.field "iss" Decode.string)
        |: Decode.maybe (Decode.field "aud" Decode.string)
        |: Decode.maybe
            (Decode.map
                (Date.fromTime << toFloat << ((*) 1000))
                (Decode.field "exp" Decode.int)
            )
        |: Decode.maybe
            (Decode.map
                (Date.fromTime << toFloat << ((*) 1000))
                (Decode.field "iat" Decode.int)
            )
        |: Decode.maybe (Decode.field "jti" Decode.string)
        |: (Decode.field "scopes" (Decode.list Decode.string))


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


decodeToken : Maybe String -> Maybe Token
decodeToken maybeToken =
    case maybeToken of
        Nothing ->
            Nothing

        Just token ->
            Result.toMaybe <| Jwt.decodeToken tokenDecoder token


decodeCredentials : Encode.Value -> Maybe Credentials
decodeCredentials val =
    Decode.decodeValue credentialsDecoder val |> Result.toMaybe


refreshTimeFromToken : Maybe Token -> Maybe Date
refreshTimeFromToken maybeToken =
    let
        maybeDate =
            Maybe.map (\token -> token.exp) maybeToken |> Maybe.Extra.join
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
            decodeToken response.token

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
            [ Navigation.newUrl model.forwardLocation
            , delayedRefreshCmd model_
            ]
        )


refreshResponse : Model.AuthResponse -> Model -> ( Model, Cmd Msg )
refreshResponse (Model.AuthResponse response) (Model model) =
    let
        decodedToken =
            decodeToken response.token

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


logoutResponse : Model -> ( Model, Cmd Msg )
logoutResponse (Model model) =
    ( Model { model | token = Nothing, authState = authStateFromToken Nothing }
    , Cmd.batch [ Navigation.newUrl model.logoutLocation ]
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
            , Cmd.batch [ Navigation.newUrl model.logoutLocation ]
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


{-| Updates the forward location.
-}
updateForwardLocation : String -> Model -> Model
updateForwardLocation location (Model model) =
    Model { model | forwardLocation = location }
