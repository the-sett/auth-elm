module AuthController
    exposing
        ( Model
        , Msg
        , init
        , logonAttempted
        , update
        , updateFromAuthCmd
        , extractAuthState
        )

{-|
Maintains the auth state and follows the TEA pattern to provide a stateful auth
module that can be wired in to TEA applications update cycles.
@docs Model, Msg
@docs init, logonAttempted, update, updateFromAuthCmd, extractAuthState
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
import Auth exposing (Credentials, isLoggedIn)
import Internal exposing (AuthCmd, AuthState)


{-| Describes the events this controller responds to.
-}
type Msg
    = AuthApi Auth.Service.Msg
    | LogIn (Maybe Credentials)
    | Refresh
    | LogOut
    | NotAuthed
    | Refreshed (Result.Result Http.Error Model.AuthResponse)


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
        , logonAttempted : Bool
        }


{-| The initial unauthed state.
-}
init :
    { forwardLocation : String
    , logoutLocation : String
    }
    -> Model
init config =
    Model
        { token = Nothing
        , decodedToken = Nothing
        , refreshFrom = Nothing
        , errorMsg = ""
        , authState = notAuthedState
        , forwardLocation = config.forwardLocation
        , logoutLocation = config.logoutLocation
        , logonAttempted = False
        }


{-|
Extracts the publicly visible auth state from the model.
-}
extractAuthState : Model -> Auth.AuthState
extractAuthState (Model model) =
    model.authState


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
    { login = login
    , refresh = refresh
    , logout = logout
    , error = \_ -> \model -> ( model, Cmd.none )
    }


login : Model.AuthResponse -> Model -> ( Model, Cmd Msg )
login (Model.AuthResponse response) (Model model) =
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


refresh : Model.AuthResponse -> Model -> ( Model, Cmd Msg )
refresh (Model.AuthResponse response) (Model model) =
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


logout : Model -> ( Model, Cmd Msg )
logout (Model model) =
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
            tokenExpiryTask refreshDate
                |> Task.attempt (\result -> Refreshed result)


tokenExpiryTask : Date -> Task.Task Http.Error Model.AuthResponse
tokenExpiryTask refreshDate =
    let
        delay refreshDate now =
            max 0 ((Date.toTime refreshDate) - now)
    in
        Time.now
            |> andThen (\now -> Process.sleep <| delay refreshDate now)
            |> andThen (\_ -> Http.toTask Auth.Service.refreshTask)


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
    case (Debug.log "auth" msg) of
        AuthApi action_ ->
            Auth.Service.update callbacks action_ (Model model)

        LogIn maybeCredentials ->
            case maybeCredentials of
                Nothing ->
                    ( Model model, Cmd.none )

                Just credentials ->
                    ( Model
                        { model
                            | token = Nothing
                            , authState = authStateFromToken Nothing
                            , logonAttempted = True
                        }
                    , Auth.Service.invokeLogin AuthApi (authRequestFromCredentials credentials)
                    )

        Refresh ->
            ( Model { model | logonAttempted = False }, Auth.Service.invokeRefresh AuthApi )

        LogOut ->
            ( Model { model | logonAttempted = False }, Auth.Service.invokeLogout AuthApi )

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
                    if isLoggedIn model.authState then
                        refresh authResponse (Model model)
                    else
                        ( Model model, Cmd.none )


{-|
Processes an AuthCmd representing a side effect request to perform some auth action
and to update the state.
-}
updateFromAuthCmd : AuthCmd -> Model -> ( Model, Cmd Msg )
updateFromAuthCmd authCmd =
    case authCmd of
        Internal.Login credentials ->
            credentials |> Just |> LogIn |> update

        Internal.Refresh ->
            update Refresh

        Internal.Logout ->
            update LogOut

        Internal.Unauthed ->
            update NotAuthed
