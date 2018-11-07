module Main exposing (main)

import Browser
import Top exposing (Model, Msg, init, update, view)


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = \_ -> Sub.none
        , view = view
        }
