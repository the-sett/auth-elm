module AuthState exposing (..)

import StateMachine exposing (..)


-- All
-- authApiRoot : String
--
-- Attempted/Failed/LoggedOut
--
-- LoggedIn/Refreshing
-- token :  String
-- decodedToken : Token
-- permissions : List String
-- expiresAt : Maybe Date
-- username : String
--


type Auth
    = Start
    | Attempted
    | Failed
    | LoggedIn
    | Refreshing
