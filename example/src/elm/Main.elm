module Main exposing (main)

import Html
import Top exposing (Model, Msg, init, update, view)


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , update = update
        , subscriptions = \_ -> Sub.none
        , view = view
        }
