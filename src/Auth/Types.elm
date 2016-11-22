module Auth.Types exposing (..)

{-|
Types for the auth module.
-}

import Date exposing (Date)
import Http
import Result
import Model
import Auth.Service


type alias Credentials =
    { username : String
    , password : String
    }


type Msg
    = AuthApi (Auth.Service.Msg)
    | LogIn (Maybe Credentials)
    | Refresh
    | LogOut
    | NotAuthed
    | Refreshed (Result.Result Http.Error Model.AuthResponse)



{--Describes the state of the authorization module at runtime. --}


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



{--
 Describes the part of the authorization state that can be preserved and
 restored accross application lifecycles.
--}


type alias SavedModel =
    { token : Maybe String }
