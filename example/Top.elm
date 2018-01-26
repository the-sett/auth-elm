module Top exposing (init, update, view, Model, Msg)

{-| The content editor client top module.

@docs init, update, subscriptions, view, Model, Msg

-}

import Dict exposing (Dict)
import Login
import Authenticated
import Html exposing (Html, div)
import Html.Attributes
import Config exposing (config)
import Auth
import Maybe.Extra
import UpdateUtils exposing (lift, message)
import TopState as TopState
    exposing
        ( Session(..)
        , initial
        , toWelcomeWithLoginModel
        , toWelcome
        , toFailedAuth
        , toAuthenticated
        , updateLoginModel
        )
import StateMachine exposing (State)


{-| The content editor program model.
-}
type alias Model =
    { auth : Auth.Model
    , session : Session
    }


{-| The content editor program top-level message types.
-}
type Msg
    = AuthMsg Auth.Msg
    | LoginMsg Login.Msg



-- Initialization


{-| Initiales the application state by setting it to the 'Initial' state.
Requests that an Auth refresh be performed to check what the current
authentication state is, as the application may be able to re-authenticate
from a refresh token held as a cookie, without needing the user to log in.
-}
init : ( Model, Cmd Msg )
init =
    ( { auth =
            Auth.init
                { authApiRoot = config.authRoot
                }
      , session = initial
      }
    , message <| AuthMsg Auth.refresh
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update action model =
    case ( model.session, action ) of
        ( _, AuthMsg msg ) ->
            let
                ( authUpdatedModel, authUpdateCmds ) =
                    lift .auth (\m x -> { m | auth = x }) AuthMsg Auth.update msg model
            in
                ( updateSessionFromAuthState authUpdatedModel, authUpdateCmds )

        ( Welcome state, LoginMsg msg ) ->
            let
                ( newState, cmdMsgs, maybeAuthCmd ) =
                    updateLoginMsg msg state
            in
                case maybeAuthCmd of
                    Nothing ->
                        ( { model | session = Welcome newState }, cmdMsgs )

                    Just authCmd ->
                        ( { model | session = Welcome newState }, Cmd.batch [ message (AuthMsg authCmd), cmdMsgs ] )

        ( FailedAuth state, LoginMsg msg ) ->
            let
                ( newState, cmdMsgs, maybeAuthCmd ) =
                    updateLoginMsg msg state
            in
                case maybeAuthCmd of
                    Nothing ->
                        ( { model | session = FailedAuth newState }, cmdMsgs )

                    Just authCmd ->
                        ( { model | session = FailedAuth newState }, Cmd.batch [ message (AuthMsg authCmd), cmdMsgs ] )

        ( _, _ ) ->
            ( model, Cmd.none )


updateSessionFromAuthState : Model -> Model
updateSessionFromAuthState model =
    let
        isAuthenticated =
            Debug.log "isAuthenticated" <|
                Auth.isLoggedIn model.auth

        logonAttempted =
            Debug.log "logonAttempted" <|
                Auth.logonAttempted model.auth

        session =
            case ( model.session, isAuthenticated, logonAttempted ) of
                ( Welcome state, True, _ ) ->
                    toAuthenticated state

                ( Welcome state, False, True ) ->
                    toFailedAuth state

                -- else if not refreshAttempted then
                --     ( Initial, Cmd.none )
                ( FailedAuth state, _, _ ) ->
                    toWelcome state

                ( Initial state, _, _ ) ->
                    toWelcomeWithLoginModel Login.init state

                ( _, _, _ ) ->
                    model.session
    in
        { model | session = session }


updateLoginMsg : Login.Msg -> State t Login.Model -> ( State t Login.Model, Cmd Msg, Maybe Auth.Msg )
updateLoginMsg msg state =
    case Login.update msg (TopState.untag state) of
        ( loginModel, cmd, maybeAuthCmd ) ->
            ( updateLoginModel (always loginModel) state
            , Cmd.map LoginMsg cmd
            , maybeAuthCmd
            )



-- View


{-| Top level view function.
-}
view : Model -> Html Msg
view model =
    let
        innerHtml =
            case model.session of
                Initial _ ->
                    Html.div [] []

                Welcome state ->
                    Login.loginView (TopState.untag state) |> Html.map LoginMsg

                FailedAuth state ->
                    Login.notPermittedView (TopState.untag state) |> Html.map LoginMsg

                Authenticated state ->
                    Authenticated.authenticatedView

        styleLink cssFileName =
            Html.node "link"
                [ Html.Attributes.rel "stylesheet"
                , Html.Attributes.href <| "styles/" ++ cssFileName
                ]
                []
    in
        div []
            [ styleLink "roboto.css"
            , styleLink "material-icons.css"
            , styleLink "material.green-indigo.min.css"
            , styleLink "main.css"
            , styleLink "auth-service.css"
            , innerHtml
            ]