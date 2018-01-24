module Main
    exposing
        ( delta2url
        , location2messages
        , init
        , update
        , subscriptions
        , view
        , Model
        , Msg
        )

{-| The content editor client top module.

@docs delta2url, location2messages, init, update, subscriptions, view, Model, Msg

-}

import Dict exposing (Dict)
import Renderer.Flexi exposing (Layout, Template)
import RouteUrl as Routing
import Editor.ContentEditor as CE
import TimeTravel.Navigation as TimeTravel
import AuthController
import Login
import Utils exposing (lift)
import StateModel exposing (boolToMaybe, (>&&>), (>||>), (>##>), defaultTransition, mapWhenCompose)
import Navigation
import Optional exposing (optional, required, when)
import Html exposing (Html)
import Config exposing (config)
import Auth
import Maybe.Extra
import ResizeObserver
import ScrollPort
import Client.TopState as TopState
    exposing
        ( Session(..)
        , WithWelcome
        , WithContentEditor
        , initial
        , toWelcomeWithWelcome
        , toWelcome
        , toFailedAuth
        , toAuthenticatedWithContentEditor
        , updateWelcome
        , updateContentEditor
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
    | WelcomeMsg Welcome.Auth.Msg
    | ContentEditorMsg CE.Msg



-- Initialization


{-| Initiales the application state by setting it to the 'Initial' state. Requests
that an Auth refreshed be performed to check what the current authentication
state is.
-}
init : ( Model, Cmd Msg )
init =
    ( { auth = setLoginLocations AuthController.init
      , session = initial
      }
    , Auth.refresh
    )


setLoginLocations authState =
    { authState | logoutLocation = "#/welcome", forwardLocation = "" }



-- Subscriptions


{-| Sets up the subscriptions for the content editor.
-}
subscriptions : ResizeObserver.Resize -> ScrollPort.Scroll -> Model -> Sub Msg
subscriptions resize scroll model =
    Sub.batch
        (optional
            [ Sub.map AuthMsg (AuthController.subscriptions model.auth) |> required
            , case model.session of
                Authenticated state ->
                    CE.subscriptions resize scroll (TopState.untag state).contentEditor
                        |> Sub.map ContentEditorMsg
                        |> Just

                _ ->
                    Nothing
            ]
        )



-- Navigation


{-| Sets the navigation bar location dependant on the state of the model.
-}
delta2url : Model -> Model -> Maybe Routing.UrlChange
delta2url _ model =
    case model.session of
        Initial _ ->
            { entry = Routing.NewEntry
            , url = ""
            }
                |> Just

        Welcome _ ->
            { entry = Routing.NewEntry
            , url = "#/welcome"
            }
                |> Just

        FailedAuth _ ->
            { entry = Routing.NewEntry
            , url = "#/welcome"
            }
                |> Just

        Authenticated state ->
            CE.delta2url (TopState.untag state).contentEditor
                (TopState.untag state).contentEditor


{-| Process naviagation bar location changes.
-}
location2messages : Navigation.Location -> List Msg
location2messages location =
    if location.hash == "" || location.hash == "#/welcome" then
        []
    else
        CE.location2messages (Debug.log "location2messages" location) |> List.map ContentEditorMsg



-- Model updates


debugFilter : Msg -> Msg
debugFilter msg =
    case msg of
        WelcomeMsg _ ->
            msg

        ContentEditorMsg _ ->
            msg

        _ ->
            Debug.log "main" msg


{-| Processes state updates for the content editor.
-}
update : Msg -> Model -> ( Model, Cmd Msg )
update action model =
    case ( model.session, (debugFilter action) ) of
        ( _, AuthMsg msg ) ->
            updateAuthMsg msg model

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

        ( Authenticated state, ContentEditorMsg msg ) ->
            let
                ( newState, cmdMsgs ) =
                    updateContentEditorMsg msg state
            in
                ( { model | session = Authenticated newState }, cmdMsgs )

        ( _, _ ) ->
            ( model, Cmd.none )


updateAuthMsg : AuthController.Msg -> Model -> ( Model, Cmd Msg )
updateAuthMsg msg model =
    let
        ( authUpdatedModel, authUpdateCmds ) =
            lift .auth (\m x -> { m | auth = x }) AuthMsg AuthController.update msg model

        isAuthenticated =
            AuthController.isLoggedIn authUpdatedModel.auth.authState

        logonAttempted =
            AuthController.logonAttempted authUpdatedModel.auth

        hasPermission =
            AuthController.hasPermission "content-author" authUpdatedModel.auth.authState

        ( session, initCmds ) =
            case ( model.session, isAuthenticated, hasPermission, logonAttempted ) of
                ( Welcome state, True, True, _ ) ->
                    let
                        ( contentEditor, editorInitCmds ) =
                            CE.init config authUpdatedModel.auth.authState.username
                    in
                        ( toAuthenticatedWithContentEditor { contentEditor = contentEditor } state
                        , editorInitCmds |> Cmd.map ContentEditorMsg
                        )

                ( Welcome state, True, False, _ ) ->
                    ( toFailedAuth state, Cmd.none )

                ( Welcome state, False, _, True ) ->
                    ( toFailedAuth state, Cmd.none )

                -- else if not refreshAttempted then
                --     ( Initial, Cmd.none )
                ( FailedAuth state, _, _, _ ) ->
                    ( toWelcome state, Cmd.none )

                ( Initial state, _, _, _ ) ->
                    ( toWelcomeWithWelcome { welcome = Welcome.Auth.init } state, Cmd.none )

                ( Authenticated state, _, _, _ ) ->
                    ( toWelcomeWithWelcome { welcome = Welcome.Auth.init } state, Cmd.none )

                ( _, _, _, _ ) ->
                    ( model.session, Cmd.none )
    in
        ( { authUpdatedModel | session = session }, Cmd.batch [ authUpdateCmds, initCmds ] )


updateWelcomeMsg : Welcome.Auth.Msg -> State t WithWelcome -> ( State t WithWelcome, Cmd Msg )
updateWelcomeMsg msg state =
    case Welcome.Auth.update msg (TopState.untag state).welcome of
        ( welcome, cmd ) ->
            ( updateWelcome (always { welcome = welcome }) state
            , Cmd.map WelcomeMsg cmd
            )


updateContentEditorMsg : CE.Msg -> State t WithContentEditor -> ( State t WithContentEditor, Cmd Msg )
updateContentEditorMsg msg state =
    case CE.update msg (TopState.untag state).contentEditor of
        ( contentEditor, cmd ) ->
            ( updateContentEditor (always { contentEditor = contentEditor }) state
            , Cmd.map ContentEditorMsg cmd
            )



-- View


{-| Top level view function for the content editor SPA.
-}
view :
    Dict String (Layout CE.Msg)
    -> Dict String (Template CE.Msg)
    -> Model
    -> Html Msg
view layouts templates model =
    case model.session of
        Initial _ ->
            Html.div [] []

        Welcome state ->
            Welcome.Auth.loginView (TopState.untag state).welcome |> Html.map WelcomeMsg

        FailedAuth state ->
            Welcome.Auth.notPermittedView (TopState.untag state).welcome |> Html.map WelcomeMsg

        Authenticated state ->
            CE.view layouts templates (TopState.untag state).contentEditor |> Html.map ContentEditorMsg
