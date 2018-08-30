module Auth.Service exposing (invokeLogin, invokeLogout, invokeRefresh, loginTask, logoutTask, refreshTask, routes)

import Http
import Json.Encode as Encode exposing (..)
import Model exposing (..)
import Platform.Cmd exposing (Cmd)
import Result
import Task exposing (Task)


invokeLogin : String -> (Result.Result Http.Error Model.AuthResponse -> msg) -> Model.AuthRequest -> Cmd msg
invokeLogin root tagger request =
    loginTask root request
        |> Http.send tagger


invokeRefresh : String -> (Result.Result Http.Error Model.AuthResponse -> msg) -> Cmd msg
invokeRefresh root tagger =
    refreshTask root
        |> Http.send tagger


invokeLogout : String -> (Result.Result Http.Error () -> msg) -> Cmd msg
invokeLogout root tagger =
    logoutTask root
        |> Http.send tagger


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
