module Model exposing (AuthRequest(..), AuthResponse(..), RefreshRequest(..), authRequestDecoder, authRequestEncoder, authResponseDecoder, authResponseEncoder, refreshRequestDecoder, refreshRequestEncoder)

import Dict exposing (Dict)
import Json.Decode as Decode exposing (..)
import Json.Decode.Extra exposing (andMap, withDefault)
import Json.Encode as Encode exposing (..)
import Set exposing (Set)


type AuthRequest
    = AuthRequest
        { username : String
        , password : String
        }


authRequestEncoder : AuthRequest -> Encode.Value
authRequestEncoder (AuthRequest model) =
    [ (\username -> ( "username", Encode.string username )) model.username
    , (\password -> ( "password", Encode.string password )) model.password
    ]
        |> Encode.object


authRequestDecoder : Decoder AuthRequest
authRequestDecoder =
    Decode.succeed
        (\username password ->
            AuthRequest
                { username = username
                , password = password
                }
        )
        |> andMap (field "username" Decode.string)
        |> andMap (field "password" Decode.string)


type RefreshRequest
    = RefreshRequest
        { refreshToken : String
        }


refreshRequestEncoder : RefreshRequest -> Encode.Value
refreshRequestEncoder (RefreshRequest model) =
    [ (\refreshToken -> ( "refreshToken", Encode.string refreshToken )) model.refreshToken
    ]
        |> Encode.object


refreshRequestDecoder : Decoder RefreshRequest
refreshRequestDecoder =
    Decode.succeed
        (\refreshToken ->
            RefreshRequest
                { refreshToken = refreshToken
                }
        )
        |> andMap (field "refreshToken" Decode.string)


type AuthResponse
    = AuthResponse
        { token : String
        , refreshToken : String
        }


authResponseEncoder : AuthResponse -> Encode.Value
authResponseEncoder (AuthResponse model) =
    [ (\token -> ( "token", Encode.string token )) model.token
    , (\refreshToken -> ( "refreshToken", Encode.string refreshToken )) model.refreshToken
    ]
        |> Encode.object


authResponseDecoder : Decoder AuthResponse
authResponseDecoder =
    Decode.succeed
        (\token refreshToken ->
            AuthResponse
                { token = token
                , refreshToken = refreshToken
                }
        )
        |> andMap (field "token" Decode.string)
        |> andMap (field "refreshToken" Decode.string)
