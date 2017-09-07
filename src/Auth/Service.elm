module Auth.Service exposing (..)

import Platform.Cmd exposing (Cmd)
import Result
import Http
import Json.Encode as Encode exposing (..)
import Task exposing (Task)
import Model exposing (..)


type Msg
    = Login (Result.Result Http.Error Model.AuthResponse)
    | Refresh (Result.Result Http.Error Model.AuthResponse)
    | Logout (Result.Result Http.Error ())


invokeLogin : String -> (Msg -> msg) -> Model.AuthRequest -> Cmd msg
invokeLogin root msg request =
    loginTask root request
        |> Http.send Login
        |> Cmd.map msg


invokeRefresh : String -> (Msg -> msg) -> Cmd msg
invokeRefresh root msg =
    refreshTask root
        |> Http.send Refresh
        |> Cmd.map msg


invokeLogout : String -> (Msg -> msg) -> Cmd msg
invokeLogout root msg =
    logoutTask root
        |> Http.send Logout
        |> Cmd.map msg


type alias Callbacks model msg =
    { login : Model.AuthResponse -> model -> ( model, Cmd msg )
    , refresh : Model.AuthResponse -> model -> ( model, Cmd msg )
    , logout : model -> ( model, Cmd msg )
    , error : Http.Error -> model -> ( model, Cmd msg )
    }


callbacks : Callbacks model msg
callbacks =
    { login = \_ -> \model -> ( model, Cmd.none )
    , refresh = \_ -> \model -> ( model, Cmd.none )
    , logout = \model -> ( model, Cmd.none )
    , error = \_ -> \model -> ( model, Cmd.none )
    }


update : Callbacks model msg -> Msg -> model -> ( model, Cmd msg )
update callbacks action model =
    case (Debug.log "account.api" action) of
        Login result ->
            (case result of
                Ok response ->
                    callbacks.login response model

                Err httpError ->
                    callbacks.error httpError model
            )

        Refresh result ->
            (case result of
                Ok response ->
                    callbacks.refresh response model

                Err httpError ->
                    callbacks.error httpError model
            )

        Logout result ->
            (case result of
                Ok _ ->
                    callbacks.logout model

                Err httpError ->
                    callbacks.error httpError model
            )


routes root =
    { loginUrl = root ++ "login"
    , logoutUrl = root ++ "logout"
    , refreshUrl = root ++ "refresh"
    }


loginTask : String -> AuthRequest -> Http.Request AuthResponse
loginTask root model =
    Http.request
        { method = "POST"
        , headers = []
        , url = routes root |> .loginUrl
        , body = Http.jsonBody <| authRequestEncoder model
        , expect = Http.expectJson authResponseDecoder
        , timeout = Nothing
        , withCredentials = False
        }


refreshTask : String -> Http.Request AuthResponse
refreshTask root =
    Http.request
        { method = "GET"
        , headers = []
        , url = routes root |> .refreshUrl
        , body = Http.emptyBody
        , expect = Http.expectJson authResponseDecoder
        , timeout = Nothing
        , withCredentials = False
        }


logoutTask : String -> Http.Request ()
logoutTask root =
    Http.request
        { method = "POST"
        , headers = []
        , url = routes root |> .logoutUrl
        , body = Http.emptyBody
        , expect = Http.expectStringResponse (\_ -> Ok ())
        , timeout = Nothing
        , withCredentials = False
        }
