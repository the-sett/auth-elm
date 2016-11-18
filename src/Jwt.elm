module Jwt exposing (JwtError(..), decodeToken, isExpired)

import Base64
import String
import Time exposing (Time)
import Json.Decode as Json exposing ((:=), Value)


type JwtError
    = TokenExpired
    | TokenProcessingError String
    | TokenDecodeError String


decodeToken : Json.Decoder a -> String -> Result JwtError a
decodeToken dec s =
    let
        f1 =
            String.split "." <| unurl s

        f2 =
            List.map fixlength f1
    in
        case f2 of
            _ :: (Result.Err e) :: _ :: [] ->
                Result.Err e

            _ :: (Result.Ok encBody) :: _ :: [] ->
                case Base64.decode encBody of
                    Result.Ok body ->
                        case Json.decodeString dec (Debug.log "jwt" body) of
                            Result.Ok x ->
                                Result.Ok x

                            Result.Err e ->
                                Result.Err (TokenDecodeError e)

                    Result.Err e ->
                        Result.Err (TokenProcessingError e)

            _ ->
                Result.Err <| TokenProcessingError "Token has invalid shape"


isExpired : Time -> String -> Bool
isExpired now token =
    case decodeToken ("exp" := Json.float) token of
        Result.Ok exp ->
            now > (exp * 1000)

        Result.Err _ ->
            True


unurl : String -> String
unurl =
    let
        fix c =
            case c of
                '-' ->
                    '+'

                '_' ->
                    '/'

                c ->
                    c
    in
        String.map fix


fixlength : String -> Result JwtError String
fixlength s =
    case String.length s % 4 of
        0 ->
            Result.Ok s

        2 ->
            Result.Ok <| String.concat [ s, "==" ]

        3 ->
            Result.Ok <| String.concat [ s, "=" ]

        _ ->
            Result.Err <| TokenProcessingError "Wrong length"
