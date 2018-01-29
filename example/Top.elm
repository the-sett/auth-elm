module Top exposing (init, update, view, Model, Msg)

{-| The content editor client top module.

@docs init, update, subscriptions, view, Model, Msg

-}

import Dict exposing (Dict)
import Html exposing (Html, div, img, h4, text, form, span)
import Html.Attributes exposing (class, src, action)
import Config exposing (config)
import Auth exposing (AuthenticationState(..))
import Maybe.Extra
import UpdateUtils exposing (lift, message)
import StateMachine exposing (State)
import ViewUtils
import Material
import Material.Button as Button
import Material.Icon as Icon
import Material.Textfield as Textfield
import Material.Options as Options
import Material.Chip as Chip
import Material.List as Lists


{-| The content editor program model.
-}
type alias Model =
    { auth : Auth.Model
    , mdl : Material.Model
    , username : String
    , password : String
    }


{-| The content editor program top-level message types.
-}
type Msg
    = AuthMsg Auth.Msg
    | Mdl (Material.Msg Msg)
    | LogIn
    | TryAgain
    | UpdateUsername String
    | UpdatePassword String



-- Initialization


{-| Initializes the application state by setting it to the default Auth state
of LoggedOut.
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
      , mdl = Material.model
      , username = ""
      , password = ""
      }
    , message <| AuthMsg Auth.refresh
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update action model =
    case action of
        Mdl action_ ->
            Material.update Mdl action_ model

        AuthMsg msg ->
            lift .auth (\m x -> { m | auth = x }) AuthMsg Auth.update msg model

        LogIn ->
            ( model, message (AuthMsg <| Auth.login { username = model.username, password = model.password }) )

        TryAgain ->
            ( { model | username = "", password = "", mdl = Material.model }, message <| AuthMsg Auth.unauthed )

        UpdateUsername str ->
            ( { model | username = str }, Cmd.none )

        UpdatePassword str ->
            ( { model | password = str }, Cmd.none )



-- View


{-| Top level view function.
-}
view : Model -> Html Msg
view model =
    let
        innerHtml =
            case model.auth.state of
                LoggedOut ->
                    loginView model

                Failed ->
                    notPermittedView model

                LoggedIn state ->
                    authenticatedView model state

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


loginView :
    { a
        | mdl : Material.Model
        , username : String
        , password : String
    }
    -> Html Msg
loginView model =
    div []
        [ div [ class "layout-fixed-width--one-card" ]
            [ ViewUtils.rhythm1SpacerDiv
            , div [ class "mdl-grid" ]
                [ div [ class "mdl-cell mdl-cell--12-col mdl-cell--8-col-tablet mdl-cell--4-col-phone mdl-card mdl-shadow--3dp" ]
                    [ div [ class "mdl-card__media" ]
                        [ img [ src "images/data_center-large.png" ]
                            []
                        ]
                    , div [ class "mdl-card__title" ]
                        [ h4 [ class "mdl-card__title-text" ]
                            [ text "Log In" ]
                        ]
                    , div [ class "mdl-card__supporting-text" ]
                        [ Textfield.render Mdl
                            [ 1, 1 ]
                            model.mdl
                            [ Textfield.label "Username"
                            , Textfield.floatingLabel
                            , Textfield.text_
                            , Textfield.value model.username
                            , Options.onInput UpdateUsername
                            ]
                            []
                        , Textfield.render Mdl
                            [ 1, 2 ]
                            model.mdl
                            [ Textfield.label "Password"
                            , Textfield.floatingLabel
                            , Textfield.text_
                            , Textfield.password
                            , Textfield.value model.password
                            , Options.onInput UpdatePassword
                            ]
                            []
                        ]
                    , div [ class "mdl-card__actions" ]
                        [ div [ class "control-bar" ]
                            [ div [ class "control-bar__row" ]
                                [ div [ class "control-bar__left-0" ]
                                    [ Button.render Mdl
                                        [ 1, 2 ]
                                        model.mdl
                                        [ Button.colored
                                        , Options.onClick LogIn
                                        ]
                                        [ text "Log In"
                                        , Icon.i "chevron_right"
                                        ]
                                    ]
                                ]
                            ]
                        ]
                    ]
                ]
            ]
        ]


notPermittedView :
    { a
        | mdl : Material.Model
        , username : String
        , password : String
    }
    -> Html Msg
notPermittedView model =
    div []
        [ div [ class "layout-fixed-width--one-card" ]
            [ ViewUtils.rhythm1SpacerDiv
            , div [ class "mdl-grid" ]
                [ div [ class "mdl-cell mdl-cell--12-col mdl-cell--8-col-tablet mdl-cell--4-col-phone mdl-card mdl-shadow--3dp" ]
                    [ div [ class "mdl-card__media" ]
                        [ img [ src "images/data_center-large.png" ]
                            []
                        ]
                    , div [ class "mdl-card__title" ]
                        [ h4 [ class "mdl-card__title-text" ]
                            [ text "Not Authorized" ]
                        ]
                    , div [ class "mdl-card__supporting-text" ]
                        [ Textfield.render Mdl
                            [ 1, 1 ]
                            model.mdl
                            [ Textfield.label "Username"
                            , Textfield.floatingLabel
                            , Textfield.text_
                            , Textfield.disabled
                            , Textfield.value model.username
                            ]
                            []
                        , Textfield.render Mdl
                            [ 1, 2 ]
                            model.mdl
                            [ Textfield.label "Password"
                            , Textfield.floatingLabel
                            , Textfield.text_
                            , Textfield.password
                            , Textfield.disabled
                            , Textfield.value model.password
                            ]
                            []
                        ]
                    , div [ class "mdl-card__actions" ]
                        [ div [ class "control-bar" ]
                            [ div [ class "control-bar__row" ]
                                [ div [ class "control-bar__left-0" ]
                                    [ Button.render Mdl
                                        [ 2, 1 ]
                                        model.mdl
                                        [ Button.colored
                                        , Options.onClick TryAgain
                                        ]
                                        [ Icon.i "chevron_left"
                                        , text "Try Again"
                                        ]
                                    ]
                                ]
                            ]
                        ]
                    ]
                ]
            ]
        ]


authenticatedView :
    { a | mdl : Material.Model, username : String }
    -> { permissions : List String, username : String }
    -> Html Msg
authenticatedView model user =
    div []
        [ div [ class "layout-fixed-width--one-card" ]
            [ ViewUtils.rhythm1SpacerDiv
            , div [ class "mdl-grid" ]
                [ div [ class "mdl-cell mdl-cell--12-col mdl-cell--8-col-tablet mdl-cell--4-col-phone mdl-card mdl-shadow--3dp" ]
                    [ div [ class "mdl-card__media" ]
                        [ img [ src "images/data_center-large.png" ]
                            []
                        ]
                    , div [ class "mdl-card__title" ]
                        [ h4 [ class "mdl-card__title-text" ]
                            [ text "Authenticated" ]
                        ]
                    , div [ class "mdl-card__supporting-text" ]
                        [ Lists.ul []
                            [ Lists.li [ Lists.withBody ]
                                -- NB! Required on every Lists.li containing body.
                                [ Lists.content []
                                    [ text "Logged In As"
                                    , Lists.body [] [ text model.username ]
                                    ]
                                ]
                            , Lists.li [ Lists.withBody ]
                                [ Lists.content []
                                    [ text "With Id"
                                    , Lists.body [] [ text user.username ]
                                    ]
                                ]
                            , Lists.li [ Lists.withBody ]
                                [ Lists.content []
                                    [ text "With Permissions"
                                    , Lists.body [] <| permissionsToChips user.permissions
                                    ]
                                ]
                            ]
                        ]
                    , div [ class "mdl-card__actions" ]
                        [ div [ class "control-bar" ]
                            [ div [ class "control-bar__row" ]
                                [ div [ class "control-bar__left-0" ]
                                    [ Button.render Mdl
                                        [ 2, 1 ]
                                        model.mdl
                                        [ Button.colored
                                        , Options.onClick TryAgain
                                        ]
                                        [ Icon.i "chevron_left"
                                        , text "Log Out"
                                        ]
                                    ]
                                ]
                            ]
                        ]
                    ]
                ]
            ]
        ]


permissionsToChips : List String -> List (Html Msg)
permissionsToChips permissions =
    List.map
        (\permission ->
            span [ class "mdl-chip mdl-chip__text" ]
                [ text permission ]
        )
        permissions
