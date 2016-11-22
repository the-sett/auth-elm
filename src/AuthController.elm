port module AuthController
    exposing
        ( update
        , subscriptions
        , init
        , isLoggedIn
        , logonAttempted
        , hasPermission
        , Model
        , Msg
        , AuthState
        )

{-|
Maintains the auth state and follows the TEA pattern to provide a stateful auth
module that can be linked in to TEA applications.
@docs update, subscriptions, init, isLoggedIn, logonAttempted, hasPermission
@docs Model, Msg, AuthState
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
import Json.Decode as Decode exposing (Decoder, (:=))
import Json.Decode.Extra exposing ((|:), withDefault, maybeNull)
import Elmq
import Jwt
import Utils exposing (..)
import Auth.Service
import Model


type alias Credentials =
    { username : String
    , password : String
    }


{-| Describes the events this controller responds to.
-}
type Msg
    = AuthApi (Auth.Service.Msg)
    | LogIn (Maybe Credentials)
    | Refresh
    | LogOut
    | NotAuthed
    | Refreshed (Result.Result Http.Error Model.AuthResponse)


{-| The complete state of this auth module.
-}
type alias Model =
    { token : Maybe String
    , decodedToken : Maybe Token
    , refreshFrom : Maybe Date
    , errorMsg : String
    , authState : AuthState
    , forwardLocation : String
    , logoutLocation : String
    , logonAttempted : Bool
    }


{-| A sub-section of the auth module state describing whether or not the user
is logged in, what permissions they have, and when their auth token will expire.
This is the part of the auth state that consumers of this module are interested
in.
-}
type alias AuthState =
    { loggedIn : Bool
    , permissions : List String
    , expiresAt : Maybe Date
    }


type alias Token =
    { sub : String
    , iss : Maybe String
    , aud : Maybe String
    , exp : Maybe Date
    , iat : Maybe Date
    , jti : Maybe String
    , scopes : List String
    }


{-| The initial unauthed state.
-}
init : Model
init =
    { token = Nothing
    , decodedToken = Nothing
    , refreshFrom = Nothing
    , errorMsg = ""
    , authState = notAuthedState
    , forwardLocation = ""
    , logoutLocation = ""
    , logonAttempted = False
    }



{--Helper functions over the auth model. --}


notAuthedState =
    { loggedIn = False
    , permissions = []
    , expiresAt = Nothing
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
        |: ("sub" := Decode.string)
        |: Decode.maybe ("iss" := Decode.string)
        |: Decode.maybe ("aud" := Decode.string)
        |: Decode.maybe
            (Decode.map
                (Date.fromTime << toFloat << ((*) 1000))
                ("exp" := Decode.int)
            )
        |: Decode.maybe
            (Decode.map
                (Date.fromTime << toFloat << ((*) 1000))
                ("iat" := Decode.int)
            )
        |: Decode.maybe ("jti" := Decode.string)
        |: ("scopes" := Decode.list Decode.string)


credentialsDecoder : Decoder Credentials
credentialsDecoder =
    (Decode.succeed
        (\username password ->
            { username = username
            , password = password
            }
        )
    )
        |: ("username" := Decode.string)
        |: ("password" := Decode.string)


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
            }


{-| Determines whether the user is currently logged in.
-}
isLoggedIn : AuthState -> Bool
isLoggedIn authState =
    authState.loggedIn


{-| Reports whether a logon has been attempted.
-}
logonAttempted : Model -> Bool
logonAttempted model =
    model.logonAttempted


{-| Checks if the user currently holds a named permission.
-}
hasPermission : String -> AuthState -> Bool
hasPermission permission authState =
    List.member permission authState.permissions



-- Subscriptions to the auth channels.


{-| Creates the needed subscriptions to auth events that can be triggered from
the Auth module.
-}
subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Elmq.listen "auth.login" (\value -> LogIn (decodeCredentials value))
        , Elmq.listenNaked "auth.logout" LogOut
        , Elmq.listenNaked "auth.refresh" Refresh
        , Elmq.listenNaked "auth.unauthed" NotAuthed
        ]



-- Auth REST API calls.


callbacks : Auth.Service.Callbacks Model Msg
callbacks =
    { login = login
    , refresh = refresh
    , logout = logout
    , error = \_ -> \model -> ( model, Cmd.none )
    }


login : Model.AuthResponse -> Model -> ( Model, Cmd Msg )
login (Model.AuthResponse response) model =
    let
        decodedToken =
            decodeToken response.token

        model' =
            { model
                | token = response.token
                , refreshFrom = refreshTimeFromToken decodedToken
                , decodedToken = decodedToken
                , authState = authStateFromToken decodedToken
            }
    in
        ( model'
        , Cmd.batch
            [ Navigation.newUrl model.forwardLocation
            , delayedRefreshCmd model'
            ]
        )


refresh : Model.AuthResponse -> Model -> ( Model, Cmd Msg )
refresh (Model.AuthResponse response) model =
    let
        decodedToken =
            decodeToken response.token

        model' =
            { model
                | token = response.token
                , refreshFrom = refreshTimeFromToken decodedToken
                , decodedToken = decodedToken
                , authState = authStateFromToken decodedToken
            }
    in
        ( model'
        , Cmd.batch
            [ delayedRefreshCmd model'
            ]
        )


logout : Http.Response -> Model -> ( Model, Cmd Msg )
logout response model =
    ( { model | token = Nothing, authState = authStateFromToken Nothing }
    , Cmd.batch [ Navigation.newUrl model.logoutLocation ]
    )



-- Functions for building and executing the refresh cycle task.


delayedRefreshCmd : Model -> Cmd Msg
delayedRefreshCmd model =
    case model.refreshFrom of
        Nothing ->
            Cmd.none

        Just refreshDate ->
            tokenExpiryTask refreshDate
                |> Task.perform (\error -> Refreshed (Result.Err error)) (\result -> Refreshed (Result.Ok result))


tokenExpiryTask : Date -> Task.Task Http.Error Model.AuthResponse
tokenExpiryTask refreshDate =
    let
        delay refreshDate now =
            max 0 ((Date.toTime refreshDate) - now)
    in
        Time.now
            `andThen` (\now -> Process.sleep <| delay refreshDate now)
            `andThen` (\_ -> Auth.Service.refreshTask)


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
update msg model =
    case (Debug.log "auth" msg) of
        AuthApi action' ->
            Auth.Service.update callbacks action' model

        LogIn maybeCredentials ->
            case maybeCredentials of
                Nothing ->
                    ( model, Cmd.none )

                Just credentials ->
                    ( { model
                        | token = Nothing
                        , authState = authStateFromToken Nothing
                        , logonAttempted = True
                      }
                    , Auth.Service.invokeLogin AuthApi (authRequestFromCredentials credentials)
                    )

        Refresh ->
            ( { model | logonAttempted = False }, Auth.Service.invokeRefresh AuthApi )

        LogOut ->
            ( { model | logonAttempted = False }, Auth.Service.invokeLogout AuthApi )

        NotAuthed ->
            ( { model
                | token = Nothing
                , authState = authStateFromToken Nothing
                , logonAttempted = False
              }
            , Cmd.batch [ Navigation.newUrl model.logoutLocation ]
            )

        Refreshed result ->
            case result of
                Err _ ->
                    ( model, Cmd.none )

                Ok authResponse ->
                    if isLoggedIn model.authState then
                        refresh authResponse model
                    else
                        ( model, Cmd.none )
