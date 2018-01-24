module Main
    exposing
        ( init
        , update
        , view
        , Model
        , Msg
        )

{-| The content editor client top module.

@docs delta2url, location2messages, init, update, subscriptions, view, Model, Msg

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
        , LoginState
        , initial
        , toWelcomeWithLoginState
        , toWelcome
        , toFailedAuth
        , toAuthenticated
        , updateWelcome
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
    | WelcomeMsg Login.Msg



-- Initialization


{-| Initiales the application state by setting it to the 'Initial' state. Requests
that an Auth refreshed be performed to check what the current authentication
state is.
-}
init : ( Model, Cmd Msg )
init =
    ( { auth =
            AuthController.init
                { logoutLocation = "#welcome"
                , forwardLocation = "#accounts"
                , authApiRoot = config.authRoot
                }
      , session = initial
      }
    , Cmd.none
    )


setLoginLocations authState =
    { authState | logoutLocation = "#/welcome", forwardLocation = "" }


{-| Processes state updates for the content editor.
-}
update : Msg -> Model -> ( Model, Cmd Msg )
update action model =
    case ( model.session, action ) of
        -- (Initial state, Ready) ->
        --     Auth.refresh
        -- ( _, AuthMsg msg ) ->
        --     updateAuthMsg msg model
        ( Welcome state, WelcomeMsg msg ) ->
            let
                ( newState, cmdMsgs ) =
                    updateWelcomeMsg msg state
            in
                ( { model | session = Welcome newState }, cmdMsgs )

        ( FailedAuth state, WelcomeMsg msg ) ->
            let
                ( newState, cmdMsgs ) =
                    updateWelcomeMsg msg state
            in
                ( { model | session = FailedAuth newState }, cmdMsgs )

        ( Authenticated state, WelcomeMsg msg ) ->
            let
                ( newState, cmdMsgs ) =
                    updateWelcomeMsg msg state
            in
                ( { model | session = Authenticated newState }, cmdMsgs )

        ( _, _ ) ->
            ( model, Cmd.none )



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


updateWelcomeMsg msg state =
    ( state, Cmd.none )



-- updateWelcomeMsg : Welcome.Auth.Msg -> State t WithWelcome -> ( State t WithWelcome, Cmd Msg )
-- updateWelcomeMsg msg state =
--     case Welcome.Auth.update msg (TopState.untag state).welcome of
--         ( welcome, cmd ) ->
--             ( updateWelcome (always { welcome = welcome }) state
--             , Cmd.map WelcomeMsg cmd
--             )
-- View


{-| Top level view function for the content editor SPA.
-}
view : Model -> Html Msg
view model =
    case model.session of
        Initial _ ->
            Html.div [] []

        Welcome state ->
            Login.loginView (TopState.untag state).loginState |> Html.map WelcomeMsg

        FailedAuth state ->
            Login.notPermittedView (TopState.untag state).loginState |> Html.map WelcomeMsg

        _ ->
            Html.div [] []



-- Authenticated state ->
--     Login.notPermittedView (TopState.untag state).welcome |> Html.map WelcomeMsg
