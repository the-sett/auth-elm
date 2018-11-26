module Main exposing (init, update, view, Model, Msg)

{-| The content editor client top module.

@docs init, update, subscriptions, view, Model, Msg

-}

import Auth
import Browser
import Config exposing (config)
import Css
import Css.Global
import Grid
import Html.Styled exposing (div, form, h4, img, input, label, span, styled, text, toUnstyled)
import Html.Styled.Attributes exposing (for, id, name, src)
import Html.Styled.Events exposing (onClick)
import Responsive
import Styles exposing (lg, md, sm, xl)
import TheSett.Buttons as Buttons
import TheSett.Cards as Cards
import TheSett.Debug
import TheSett.Laf as Laf exposing (devices, fonts, responsiveMeta, wrapper)
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
    | ToggleGrid



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

        ToggleGrid ->
            ( { model | debugStyle = not model.debugStyle }, Cmd.none )


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


warmMidGrey =
    Css.rgb 214 212 214


paperWhite =
    Css.rgb 248 248 248


global : List Css.Global.Snippet
global =
    [ Css.Global.each
        [ Css.Global.html ]
        [ Css.height <| Css.pct 100

        --, Css.backgroundColor <| warmMidGrey
        , Responsive.deviceStyle devices
            (\common device ->
                let
                    headerPx =
                        Responsive.rhythm 9.5 common device
                in
                Css.property "background" <|
                    "linear-gradient(rgb(120, 116, 120) 0%, "
                        ++ String.fromFloat headerPx
                        ++ "px, rgb(225, 212, 214) 0px, rgb(208, 212, 214) 100%)"
            )
        ]
    , Css.Global.typeSelector "input:focus"
        [ Css.Global.generalSiblings
            [ Css.Global.typeSelector "span:before"
                [ Css.width <| Css.pct 50
                ]
            ]
        ]
    , Css.Global.typeSelector "input:focus"
        [ Css.Global.generalSiblings
            [ Css.Global.typeSelector "span:after"
                [ Css.width <| Css.pct 50
                ]
            ]
        ]

    -- input:focus ~ .bar:before,
    -- input:focus ~ .bar:after
    ]


{-| Top level view function.
-}
view : Model -> Browser.Document Msg
view model =
    { title = "Tea Tree Example"
    , body = [ body model ]
    }


body model =
    styledBody model
        |> toUnstyled


styledBody : Model -> Html.Styled.Html Msg
styledBody model =
    let
        innerView =
            [ responsiveMeta
            , fonts
            , Laf.style devices
            , Css.Global.global global
            , case model.session of
                Initial ->
                    initialView

                LoggedOut ->
                    loginView model

                FailedAuth ->
                    notPermittedView model

                LoggedIn state ->
                    authenticatedView model state
            ]

        debugStyle =
            Css.Global.global <|
                TheSett.Debug.global Laf.devices
    in
    case model.debugStyle of
        True ->
            div [] (debugStyle :: innerView)

        False ->
            div [] innerView


card imageUrl title cardBody controls devices =
    Cards.card
        [ sm
            [ Styles.styles
                [ Css.maxWidth <| Css.vw 100
                , Css.minWidth <| Css.px 310
                , Css.backgroundColor <| paperWhite
                ]
            ]
        , md
            [ Styles.styles
                [ Css.maxWidth <| Css.px 420
                , Css.minWidth <| Css.px 400
                , Css.backgroundColor <| paperWhite
                ]
            ]
        ]
        []
        [ Cards.image
            [ Styles.height 6
            , sm [ Cards.src imageUrl ]
            ]
            []
            [ styled div
                [ Css.position Css.relative
                , Css.height <| Css.pct 100
                ]
                []
                [ Buttons.button
                    [ sm
                        [ Styles.styles
                            [ Css.position Css.absolute
                            , Css.bottom <| Css.em -3.5
                            , Css.right <| Css.em 2
                            , Css.backgroundColor <| Css.rgb 160 220 180
                            ]
                        ]
                    ]
                    [ onClick ToggleGrid ]
                    [ text "Grid" ]
                    devices
                ]
            ]
        , Cards.title title
        , Cards.body cardBody
        , Cards.controls controls
        ]
        devices


initialView : Html.Styled.Html Msg
initialView =
    styled div
        [ Css.marginTop <| Css.vh 10
        ]
        []
        [ Grid.grid
            [ sm [ Grid.columns 12 ] ]
            []
            [ Grid.row
                [ sm [ Grid.center ] ]
                []
                [ Grid.col
                    []
                    []
                    [ card "images/data_center-large.png"
                        "Attempting to Restore"
                        [ text "Attempting to restore authentication using a local refresh token." ]
                        []
                        devices
                    ]
                ]
            ]
            devices
        ]


loginView :
    { a
        | username : String
        , password : String
    }
    -> Html.Styled.Html Msg
loginView model =
    styled div
        [ Responsive.deviceStyle devices
            (\common device -> Css.marginTop <| Responsive.rhythmPx 3 common device)
        ]
        []
        [ Grid.grid
            [ sm [ Grid.columns 12 ] ]
            []
            [ Grid.row
                [ sm [ Grid.center ] ]
                []
                [ Grid.col
                    []
                    []
                    [ card "images/data_center-large.png"
                        "Log In"
                        [ loginForm
                        ]
                        [ Buttons.button [] [] [ text "Log In" ] devices
                        ]
                        devices
                    ]
                ]
            ]
            devices
        ]


loginForm =
    form []
        [ inputAndLabel
        , inputAndLabel
        ]


inputAndLabel =
    styled div
        [ Css.position Css.relative
        , Css.fontFamilies [ "Helvetica" ]
        , Responsive.deviceStyles devices
            (\common device ->
                [ Css.marginTop <| Responsive.rhythmPx 1 common device
                , Css.paddingBottom <| Responsive.rhythmPx 1 common device
                , Css.height <| Responsive.rhythmPx 1 common device
                ]
            )
        ]
        []
        [ styled label
            [ Css.position Css.absolute
            , Css.color <| Css.hex "999"
            , Css.left <| Css.px 0
            , Css.top <| Css.px 0
            , Css.property "transition" "all 0.2s ease"
            , Css.pointerEvents Css.none

            --
            , Css.top <| Css.px -26
            , Css.transform <| Css.scale 0.75
            , Css.left <| Css.px 0
            , Css.color <| Css.hex "4CAF50"
            ]
            [ for "username" ]
            [ text "Username" ]
        , styled input
            [ Css.border <| Css.px 0
            , Css.borderBottom3 (Css.px 1) Css.solid (Css.hex "666")
            , Css.display Css.block
            , Css.focus [ Css.outline Css.none ]
            , Css.backgroundColor Css.transparent
            , Css.width <| Css.pct 100
            ]
            [ id "username", name "username" ]
            []
        , styled span
            [ Css.position Css.relative
            , Css.display Css.block
            , Css.width <| Css.pct 100
            , Css.before
                [ Css.property "content" "''"
                , Css.height <| Css.px 2
                , Css.width <| Css.px 0
                , Css.bottom <| Css.px 0
                , Css.position Css.absolute
                , Css.backgroundColor <| Css.hex "4CAF50"
                , Css.property "transition" "all 0.2s ease"
                , Css.left <| Css.pct 50
                ]
            , Css.after
                [ Css.property "content" "''"
                , Css.height <| Css.px 2
                , Css.width <| Css.px 0
                , Css.bottom <| Css.px 0
                , Css.position Css.absolute
                , Css.backgroundColor <| Css.hex "4CAF50"
                , Css.property "transition" "all 0.2s ease"
                , Css.right <| Css.pct 50
                ]
            ]
            []
            []
        ]


notPermittedView :
    { a
        | username : String
        , password : String
    }
    -> Html.Styled.Html Msg
notPermittedView model =
    styled div
        [ Css.marginTop <| Css.vh 10
        ]
        []
        [ Grid.grid
            [ sm [ Grid.columns 12 ] ]
            []
            [ Grid.row
                [ sm [ Grid.center ] ]
                []
                [ Grid.col
                    []
                    []
                    [ card "images/data_center-large.png"
                        "Not Authorized"
                        [ text "Username"
                        , text "Password"
                        ]
                        [ Buttons.button [] [] [ text "Try Again" ] devices ]
                        devices
                    ]
                ]
            ]
            devices
        ]


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
