module UpdateUtils exposing (lift)

{-| Utils for update function boiler-plate.
-}


{-| Convenience function for lifting an update function for an inner model
and messages into a parent one.
-}
lift :
    (model -> submodel)
    -> (model -> submodel -> model)
    -> (subaction -> action)
    -> (subaction -> submodel -> ( submodel, Cmd subaction ))
    -> subaction
    -> model
    -> ( model, Cmd action )
lift get set fwd update action model =
    let
        ( submodel_, e ) =
            update action (get model)
    in
        ( set model submodel_, Cmd.map fwd e )
