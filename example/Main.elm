module Main exposing (init, update, view, Model, Msg)

{-| The content editor client top module.

@docs init, update, subscriptions, view, Model, Msg

-}

import Dict exposing (Dict)
import AuthController
import Login
import Html exposing (Html)
import Config exposing (config)
import Auth
import Maybe.Extra
import UpdateUtils exposing (lift)
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
    { auth : AuthController.Model
    , session : Session
    }


{-| The content editor program top-level message types.
-}
type Msg
    = AuthMsg AuthController.Msg
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
            AuthController.init
                { logoutLocation = "#welcome"
                , forwardLocation = "#welcome"
                , authApiRoot = config.authRoot
                }
      , session = initial
      }
    , Cmd.none
    )


{-| Processes state updates for the content editor.
-}
update : Msg -> Model -> ( Model, Cmd Msg )
update action model =
    case ( model.session, action ) of
        -- (Initial state, Ready) ->
        --     Auth.refresh
        -- ( _, AuthMsg msg ) ->
        --     updateAuthMsg msg model
        ( Welcome state, LoginMsg msg ) ->
            let
                ( newState, cmdMsgs ) =
                    updateLoginMsg msg state
            in
                ( { model | session = Welcome newState }, cmdMsgs )

        ( FailedAuth state, LoginMsg msg ) ->
            let
                ( newState, cmdMsgs ) =
                    updateLoginMsg msg state
            in
                ( { model | session = FailedAuth newState }, cmdMsgs )

        ( _, _ ) ->
            ( model, Cmd.none )


updateLoginMsg : Login.Msg -> State t Login.Model -> ( State t Login.Model, Cmd Msg )
updateLoginMsg msg state =
    case Login.update msg (TopState.untag state) of
        ( loginModel, cmd, authCmd ) ->
            ( updateLoginModel (always loginModel) state
            , Cmd.map LoginMsg cmd
            )



-- updateAuthMsg : AuthController.Msg -> Model -> ( Model, Cmd Msg )
-- updateAuthMsg msg model =
--     let
--         ( authUpdatedModel, authUpdateCmds ) =
--             lift .auth (\m x -> { m | auth = x }) AuthMsg AuthController.update msg model
--
--         isAuthenticated =
--             AuthController.isLoggedIn authUpdatedModel.auth.authState
--
--         logonAttempted =
--             AuthController.logonAttempted authUpdatedModel.auth
--
--         hasPermission =
--             AuthController.hasPermission "content-author" authUpdatedModel.auth.authState
--
--         ( session, initCmds ) =
--             case ( model.session, isAuthenticated, hasPermission, logonAttempted ) of
--                 ( Welcome state, True, True, _ ) ->
--                     ( toAuthenticated state, Cmd.none )
--
--                 ( Welcome state, True, False, _ ) ->
--                     ( toFailedAuth state, Cmd.none )
--
--                 ( Welcome state, False, _, True ) ->
--                     ( toFailedAuth state, Cmd.none )
--
--                 -- else if not refreshAttempted then
--                 --     ( Initial, Cmd.none )
--                 ( FailedAuth state, _, _, _ ) ->
--                     ( toWelcome state, Cmd.none )
--
--                 ( Initial state, _, _, _ ) ->
--                     ( toWelcomeWithWelcome { welcome = Welcome.Auth.init } state, Cmd.none )
--
--                 ( Authenticated state, _, _, _ ) ->
--                     ( toWelcomeWithWelcome { welcome = Welcome.Auth.init } state, Cmd.none )
--
--                 ( _, _, _, _ ) ->
--                     ( model.session, Cmd.none )
--     in
--         ( { authUpdatedModel | session = session }, Cmd.batch [ authUpdateCmds, initCmds ] )
-- View


{-| Top level view function for the content editor SPA.
-}
view : Model -> Html Msg
view model =
    case model.session of
        Initial _ ->
            Html.div [] []

        Welcome state ->
            Login.loginView (TopState.untag state) |> Html.map LoginMsg

        FailedAuth state ->
            Login.notPermittedView (TopState.untag state) |> Html.map LoginMsg

        Authenticated state ->
            Login.authenticatedView
