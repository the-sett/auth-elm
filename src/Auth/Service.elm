module Auth.Service exposing (..)

import Log
import Platform.Cmd exposing (Cmd)
import Result
import Http
import Http
import Json.Encode as Encode exposing (..)
import Task exposing (Task)
import Model exposing (..)


type Msg
    = Login (Result.Result Http.Error Model.AuthResponse)
    | Refresh (Result.Result Http.Error Model.AuthResponse)
    | Logout (Result.Result Http.Error Http.Response)


invokeLogin : (Msg -> msg) -> Model.AuthRequest -> Cmd msg
invokeLogin msg request =
    loginTask request
        |> Task.perform (\error -> Login (Result.Err error)) (\result -> Login (Result.Ok result))
        |> Cmd.map msg


invokeRefresh : (Msg -> msg) -> Cmd msg
invokeRefresh msg =
    refreshTask
        |> Task.perform (\error -> Refresh (Result.Err error)) (\result -> Refresh (Result.Ok result))
        |> Cmd.map msg


invokeLogout : (Msg -> msg) -> Cmd msg
invokeLogout msg =
    logoutTask
        |> Task.perform (\error -> Logout (Result.Err error)) (\result -> Logout (Result.Ok result))
        |> Cmd.map msg


type alias Callbacks model msg =
    { login : Model.AuthResponse -> model -> ( model, Cmd msg )
    , refresh : Model.AuthResponse -> model -> ( model, Cmd msg )
    , logout : Http.Response -> model -> ( model, Cmd msg )
    , error : Http.Error -> model -> ( model, Cmd msg )
    }


callbacks : Callbacks model msg
callbacks =
    { login = \_ -> \model -> ( model, Cmd.none )
    , refresh = \_ -> \model -> ( model, Cmd.none )
    , logout = \_ -> \model -> ( model, Cmd.none )
    , error = \_ -> \model -> ( model, Cmd.none )
    }


update : Callbacks model msg -> Msg -> model -> ( model, Cmd msg )
update callbacks action model =
    update' callbacks (Log.debug "account.api" action) model


update' : Callbacks model msg -> Msg -> model -> ( model, Cmd msg )
update' callbacks action model =
    case action of
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
                Ok status ->
                    callbacks.logout status model

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


loginTask : AuthRequest -> Task Http.Error Model.AuthResponse
loginTask model =
    { verb = "POST"
    , headers = [ ( "Content-Type", "application/json" ) ]
    , url = routes.loginUrl
    , body = Http.string <| Encode.encode 0 <| authRequestEncoder model
    }
        |> Http.send Http.defaultSettings
        |> Http.fromJson authResponseDecoder


refreshTask : Task Http.Error Model.AuthResponse
refreshTask =
    { verb = "GET"
    , headers = []
    , url = routes.refreshUrl
    , body = Http.empty
    }
        |> Http.send Http.defaultSettings
        |> Http.fromJson authResponseDecoder


logoutTask : Task Http.Error Http.Response
logoutTask =
    { verb = "POST"
    , headers = []
    , url = routes.logoutUrl
    , body = Http.empty
    }
        |> Http.send Http.defaultSettings
        |> Task.mapError promoteError


promoteError : Http.RawError -> Http.Error
promoteError rawError =
    case rawError of
        Http.RawTimeout ->
            Http.Timeout

        Http.RawNetworkError ->
            Http.NetworkError
