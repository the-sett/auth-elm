module UpdateUtils exposing (message, Update, lift)

import Dict exposing (Dict)
import Set exposing (Set)
import Maybe
import Maybe.Extra exposing (isJust)
import List
import Http
import Task


{-| A command to generate a message without performing any action.
This is useful for implementing components that generate events in the manner
of HTML elements, but where the event fires from within Elm code, rather than
by an external trigger.
-}
message : msg -> Cmd msg
message x =
    Task.perform identity (Task.succeed x)


{-| Variant of TEA update function type, where effects may be
lifted to a different type.
-}
type alias Update_ model action action_ =
    action -> model -> ( model, Cmd action_ )


{-| Standard TEA update function type.
-}
type alias Update model action =
    Update_ model action action


{-| Convenience function for writing update-function boilerplate. Example use:
case msg of
...
ButtonsMsg msg_ ->
lift .buttons (\m x->{m|buttons=x}) ButtonsMsg Demo.Buttons.update msg_ model
This is equivalent to the more verbose
case msg of
...
ButtonsMsg msg_ ->
let
(buttons_, cmd) =
Demo.Buttons.update msg_ model.buttons
in
( { model | buttons = buttons_}
, Cmd.map ButtonsMsg cmd
)
-}
lift :
    (model -> submodel)
    -> (model -> submodel -> model)
    -> (subaction -> action)
    -> Update submodel subaction
    -> subaction
    -> model
    -> ( model, Cmd action )
lift get set fwd update action model =
    let
        ( submodel_, e ) =
            update action (get model)
    in
        ( set model submodel_, Cmd.map fwd e )
