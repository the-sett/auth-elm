module Main exposing (init, update, view, Model, Msg)

{-| The content editor client top module.

@docs init, update, subscriptions, view, Model, Msg

-}

import Auth
import Config exposing (config)
import DebugStyle
import Grid
import Html.Styled exposing (div, h4, img, input, span, text, toUnstyled)
import Html.Styled.Attributes exposing (src)
import TheSettLaf exposing (devices, fonts, responsiveMeta)
import Update3
import UpdateUtils exposing (lift)
import ViewUtils


{-| The content editor program model.
-}
type alias Model =
    { auth : Auth.Model
    , session : Session
    , username : String
    , password : String
    , debugStyle : Bool
    }


type Session
    = Initial
    | LoggedOut
    | FailedAuth
    | LoggedIn
        { scopes : List String
        , subject : String
        }


{-| The content editor program top-level message types.
-}
type Msg
    = AuthMsg Auth.Msg
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
init : flags -> ( Model, Cmd Msg )
init _ =
    ( { auth =
            Auth.init
                { authApiRoot = config.authRoot
                }
      , session = Initial
      , username = ""
      , password = ""
      , debugStyle = False
      }
    , Auth.refresh |> Cmd.map AuthMsg
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update action model =
    case action of
        AuthMsg msg ->
            Update3.lift .auth (\x m -> { m | auth = x }) AuthMsg Auth.update msg model
                |> Update3.evalMaybe (\status -> \nextModel -> ( { nextModel | session = authStatusToSession status }, Cmd.none )) Cmd.none

        LogIn ->
            ( model, Auth.login { username = model.username, password = model.password } |> Cmd.map AuthMsg )

        TryAgain ->
            ( { model | username = "", password = "" }, Auth.unauthed |> Cmd.map AuthMsg )

        UpdateUsername str ->
            ( { model | username = str }, Cmd.none )

        UpdatePassword str ->
            ( { model | password = str }, Cmd.none )


authStatusToSession : Auth.Status -> Session
authStatusToSession status =
    case status of
        Auth.LoggedOut ->
            LoggedOut

        Auth.Failed ->
            FailedAuth

        Auth.LoggedIn access ->
            LoggedIn access



-- View


{-| Top level view function.
-}
view model =
    styledView model
        |> toUnstyled


styledView : Model -> Html.Styled.Html Msg
styledView model =
    let
        innerView =
            [ responsiveMeta
            , fonts
            , TheSettLaf.style devices
            , case model.session of
                Initial ->
                    initialView

                LoggedOut ->
                    --loginView model
                    initialView

                FailedAuth ->
                    notPermittedView model

                LoggedIn state ->
                    authenticatedView model state
            ]

        debugStyle =
            DebugStyle.style devices
    in
    case model.debugStyle of
        True ->
            div [] (debugStyle :: innerView)

        False ->
            div [] innerView


initialView : Html.Styled.Html Msg
initialView =
    -- div []
    --     [ div [ class "layout-fixed-width--one-card" ]
    --         [ ViewUtils.rhythm1SpacerDiv
    --         , div [ class "mdl-grid" ]
    --             [ div [ class "mdl-cell mdl-cell--12-col mdl-cell--8-col-tablet mdl-cell--4-col-phone mdl-card mdl-shadow--3dp" ]
    --                 [ div [ class "mdl-card__media" ]
    --                     [ img [ src "images/data_center-large.png" ]
    --                         []
    --                     ]
    --                 , div [ class "mdl-card__title" ]
    --                     [ h4 [ class "mdl-card__title-text" ]
    --                         [ text "Attempting to Restore" ]
    --                     ]
    --                 ]
    --             ]
    --         ]
    --     ]
    div []
        [ div []
            [ ViewUtils.rhythm1SpacerDiv
            , div []
                [ div []
                    [ div []
                        [ img [ src "images/data_center-large.png" ]
                            []
                        ]
                    , div []
                        [ h4 []
                            [ text "Attempting to Restore" ]
                        ]
                    ]
                ]
            ]
        ]


loginView :
    { a
        | username : String
        , password : String
    }
    -> Html.Styled.Html Msg
loginView model =
    -- div []
    --     [ div [ class "layout-fixed-width--one-card" ]
    --         [ ViewUtils.rhythm1SpacerDiv
    --         , div [ class "mdl-grid" ]
    --             [ div [ class "mdl-cell mdl-cell--12-col mdl-cell--8-col-tablet mdl-cell--4-col-phone mdl-card mdl-shadow--3dp" ]
    --                 [ div [ class "mdl-card__media" ]
    --                     [ img [ src "images/data_center-large.png" ]
    --                         []
    --                     ]
    --                 , div [ class "mdl-card__title" ]
    --                     [ h4 [ class "mdl-card__title-text" ]
    --                         [ text "Log In" ]
    --                     ]
    --                 , div [ class "mdl-card__supporting-text" ]
    --                     [ Textfield.render Mdl
    --                         [ 1, 1 ]
    --                         model.mdl
    --                         [ Textfield.label "Username"
    --                         , Textfield.floatingLabel
    --                         , Textfield.text_
    --                         , Textfield.value model.username
    --                         , Options.onInput UpdateUsername
    --                         ]
    --                         []
    --                     , Textfield.render Mdl
    --                         [ 1, 2 ]
    --                         model.mdl
    --                         [ Textfield.label "Password"
    --                         , Textfield.floatingLabel
    --                         , Textfield.text_
    --                         , Textfield.password
    --                         , Textfield.value model.password
    --                         , Options.onInput UpdatePassword
    --                         ]
    --                         []
    --                     ]
    --                 , div [ class "mdl-card__actions" ]
    --                     [ div [ class "control-bar" ]
    --                         [ div [ class "control-bar__row" ]
    --                             [ div [ class "control-bar__left-0" ]
    --                                 [ Button.render Mdl
    --                                     [ 1, 2 ]
    --                                     model.mdl
    --                                     [ Button.colored
    --                                     , Options.onClick LogIn
    --                                     ]
    --                                     [ text "Log In"
    --                                     , Icon.i "chevron_right"
    --                                     ]
    --                                 ]
    --                             ]
    --                         ]
    --                     ]
    --                 ]
    --             ]
    --         ]
    --     ]
    div []
        [ div []
            [ ViewUtils.rhythm1SpacerDiv
            , div []
                [ div []
                    [ div []
                        [ img [ src "images/data_center-large.png" ]
                            []
                        ]
                    , div []
                        [ h4 []
                            [ text "Log In" ]
                        ]
                    , div []
                        [ text "Username"
                        , text "Password"

                        -- Textfield.render
                        --     [ Textfield.label "Username"
                        --     , Textfield.floatingLabel
                        --     , Textfield.text_
                        --     , Textfield.value model.username
                        --     , Options.onInput UpdateUsername
                        --     ]
                        --     []
                        -- , Textfield.render
                        --     [ Textfield.label "Password"
                        --     , Textfield.floatingLabel
                        --     , Textfield.text_
                        --     , Textfield.password
                        --     , Textfield.value model.password
                        --     , Options.onInput UpdatePassword
                        --     ]
                        --     []
                        ]
                    , div []
                        [ div []
                            [ div []
                                [ div []
                                    [ text "Log In"

                                    -- Button.render
                                    --     [ Button.colored
                                    --     , Options.onClick LogIn
                                    --     ]
                                    --     [ text "Log In"
                                    --     , Icon.i "chevron_right"
                                    --     ]
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
        | username : String
        , password : String
    }
    -> Html.Styled.Html Msg
notPermittedView model =
    -- div []
    --     [ div [ class "layout-fixed-width--one-card" ]
    --         [ ViewUtils.rhythm1SpacerDiv
    --         , div [ class "mdl-grid" ]
    --             [ div [ class "mdl-cell mdl-cell--12-col mdl-cell--8-col-tablet mdl-cell--4-col-phone mdl-card mdl-shadow--3dp" ]
    --                 [ div [ class "mdl-card__media" ]
    --                     [ img [ src "images/data_center-large.png" ]
    --                         []
    --                     ]
    --                 , div [ class "mdl-card__title" ]
    --                     [ h4 [ class "mdl-card__title-text" ]
    --                         [ text "Not Authorized" ]
    --                     ]
    --                 , div [ class "mdl-card__supporting-text" ]
    --                     [ Textfield.render Mdl
    --                         [ 1, 1 ]
    --                         model.mdl
    --                         [ Textfield.label "Username"
    --                         , Textfield.floatingLabel
    --                         , Textfield.text_
    --                         , Textfield.disabled
    --                         , Textfield.value model.username
    --                         ]
    --                         []
    --                     , Textfield.render Mdl
    --                         [ 1, 2 ]
    --                         model.mdl
    --                         [ Textfield.label "Password"
    --                         , Textfield.floatingLabel
    --                         , Textfield.text_
    --                         , Textfield.password
    --                         , Textfield.disabled
    --                         , Textfield.value model.password
    --                         ]
    --                         []
    --                     ]
    --                 , div [ class "mdl-card__actions" ]
    --                     [ div [ class "control-bar" ]
    --                         [ div [ class "control-bar__row" ]
    --                             [ div [ class "control-bar__left-0" ]
    --                                 [ Button.render Mdl
    --                                     [ 2, 1 ]
    --                                     model.mdl
    --                                     [ Button.colored
    --                                     , Options.onClick TryAgain
    --                                     ]
    --                                     [ Icon.i "chevron_left"
    --                                     , text "Try Again"
    --                                     ]
    --                                 ]
    --                             ]
    --                         ]
    --                     ]
    --                 ]
    --             ]
    --         ]
    --     ]
    div [] []


authenticatedView :
    { a | username : String }
    -> { scopes : List String, subject : String }
    -> Html.Styled.Html Msg
authenticatedView model user =
    -- div []
    --     [ div [ class "layout-fixed-width--one-card" ]
    --         [ ViewUtils.rhythm1SpacerDiv
    --         , div [ class "mdl-grid" ]
    --             [ div [ class "mdl-cell mdl-cell--12-col mdl-cell--8-col-tablet mdl-cell--4-col-phone mdl-card mdl-shadow--3dp" ]
    --                 [ div [ class "mdl-card__media" ]
    --                     [ img [ src "images/data_center-large.png" ]
    --                         []
    --                     ]
    --                 , div [ class "mdl-card__title" ]
    --                     [ h4 [ class "mdl-card__title-text" ]
    --                         [ text "Authenticated" ]
    --                     ]
    --                 , div [ class "mdl-card__supporting-text" ]
    --                     [ Lists.ul []
    --                         [ Lists.li [ Lists.withBody ]
    --                             -- NB! Required on every Lists.li containing body.
    --                             [ Lists.content []
    --                                 [ text "Logged In As"
    --                                 , Lists.body [] [ text model.username ]
    --                                 ]
    --                             ]
    --                         , Lists.li [ Lists.withBody ]
    --                             [ Lists.content []
    --                                 [ text "With Id"
    --                                 , Lists.body [] [ text user.subject ]
    --                                 ]
    --                             ]
    --                         , Lists.li [ Lists.withBody ]
    --                             [ Lists.content []
    --                                 [ text "With Permissions"
    --                                 , Lists.body [] <| permissionsToChips user.scopes
    --                                 ]
    --                             ]
    --                         ]
    --                     ]
    --                 , div [ class "mdl-card__actions" ]
    --                     [ div [ class "control-bar" ]
    --                         [ div [ class "control-bar__row" ]
    --                             [ div [ class "control-bar__left-0" ]
    --                                 [ Button.render Mdl
    --                                     [ 2, 1 ]
    --                                     model.mdl
    --                                     [ Button.colored
    --                                     , Options.onClick TryAgain
    --                                     ]
    --                                     [ Icon.i "chevron_left"
    --                                     , text "Log Out"
    --                                     ]
    --                                 ]
    --                             ]
    --                         ]
    --                     ]
    --                 ]
    --             ]
    --         ]
    --     ]
    div [] []


permissionsToChips : List String -> List (Html.Styled.Html Msg)
permissionsToChips permissions =
    -- List.map
    --     (\permission ->
    --         span [ class "mdl-chip mdl-chip__text" ]
    --             [ text permission ]
    --     )
    --     permissions
    []
