module Jwt exposing
    ( JwtError(..)
    , Token
    , decode
    , decodeWithErrors
    , isExpired
    )

import Base64
import Json.Decode as Decode exposing (Decoder, Value, field)
import Json.Decode.Extra exposing (andMap, withDefault)
import Time exposing (Posix)


type JwtError
    = TokenExpired
    | TokenProcessingError String
    | TokenDecodeError String


{-| Describes the fields of a decoded JWT token.
-}
type alias Token =
    { sub : String
    , iss : Maybe String
    , aud : Maybe String
    , exp : Posix
    , iat : Maybe Posix
    , jti : Maybe String
    , scopes : List String
    }


decode : String -> Maybe Token
decode token =
    Result.toMaybe <| decodeWithErrors token


decodeWithErrors : String -> Result JwtError Token
decodeWithErrors token =
    extractAndDecodeToken tokenDecoder token


isExpired : Posix -> String -> Bool
isExpired now token =
    case extractAndDecodeToken (field "exp" Decode.int) token of
        Result.Ok exp ->
            Time.posixToMillis now > (exp * 1000)

        Result.Err _ ->
            True


tokenDecoder : Decoder Token
tokenDecoder =
    Decode.succeed
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
        |> andMap (Decode.field "sub" Decode.string)
        |> andMap (Decode.maybe (Decode.field "iss" Decode.string))
        |> andMap (Decode.maybe (Decode.field "aud" Decode.string))
        |> andMap
            (Decode.map
                (Time.millisToPosix << (*) 1000)
                (Decode.field "exp" Decode.int)
            )
        |> andMap
            (Decode.maybe
                (Decode.map
                    (Time.millisToPosix << (*) 1000)
                    (Decode.field "iat" Decode.int)
                )
            )
        |> andMap (Decode.maybe (Decode.field "jti" Decode.string))
        |> andMap (Decode.field "scopes" (Decode.list Decode.string))


extractAndDecodeToken : Decode.Decoder a -> String -> Result JwtError a
extractAndDecodeToken dec s =
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
                    case Decode.decodeString dec body of
                        Result.Ok x ->
                            Result.Ok x

                        Result.Err e ->
                            Result.Err (TokenDecodeError <| Decode.errorToString e)

                Result.Err e ->
                    Result.Err (TokenProcessingError e)

        _ ->
            Result.Err <| TokenProcessingError "Token has invalid shape"


unurl : String -> String
unurl =
    let
        fix ch =
            case ch of
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
    case modBy 4 (String.length s) of
        0 ->
            Result.Ok s

        2 ->
            Result.Ok <| String.concat [ s, "==" ]

        3 ->
            Result.Ok <| String.concat [ s, "=" ]

        _ ->
            Result.Err <| TokenProcessingError "Wrong length"
