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


invokeLogin : (Msg -> msg) -> Model.AuthRequest -> Cmd msg
invokeLogin msg request =
    loginTask request
        |> Http.send Login
        |> Cmd.map msg


invokeRefresh : (Msg -> msg) -> Cmd msg
invokeRefresh msg =
    refreshTask
        |> Http.send Refresh
        |> Cmd.map msg


invokeLogout : (Msg -> msg) -> Cmd msg
invokeLogout msg =
    logoutTask
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


api =
    "/auth/"


routes =
    { loginUrl = api ++ "login"
    , logoutUrl = api ++ "logout"
    , refreshUrl = api ++ "refresh"
    }


loginTask : AuthRequest -> Http.Request AuthResponse
loginTask model =
    Http.request
        { method = "POST"
        , headers = []
        , url = routes.loginUrl
        , body = Http.jsonBody <| authRequestEncoder model
        , expect = Http.expectJson authResponseDecoder
        , timeout = Nothing
        , withCredentials = False
        }


refreshTask : Http.Request AuthResponse
refreshTask =
    Http.request
        { method = "GET"
        , headers = []
        , url = routes.refreshUrl
        , body = Http.emptyBody
        , expect = Http.expectJson authResponseDecoder
        , timeout = Nothing
        , withCredentials = False
        }


logoutTask : Http.Request ()
logoutTask =
    Http.request
        { method = "POST"
        , headers = []
        , url = routes.logoutUrl
        , body = Http.emptyBody
        , expect = Http.expectStringResponse (\_ -> Ok ())
        , timeout = Nothing
        , withCredentials = False
        }
