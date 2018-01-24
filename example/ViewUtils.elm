module ViewUtils exposing (..)

import Html exposing (..)
import Html.Attributes exposing (title, class, action, attribute)
import Material
import Material.Options as Options exposing (Style, Property, cs, when, nop, disabled)
import Material.Button as Button
import Material.Icon as Icon
import Material.Grid as Grid
import Material.Dialog as Dialog


{-| A grid cell that is 6 columns on dektop and 4 on tablet and mobile. This
provides a 2 column layout, shrinking to 1 on mobile.
-}
column644 : List (Html msg) -> Grid.Cell msg
column644 =
    Grid.cell
        [ Grid.size Grid.Desktop 6
        , Grid.size Grid.Tablet 4
        , Grid.size Grid.Phone 4
        ]


{-| A grid cell that is full width on all devices.
-}
columnAll12 : List (Html msg) -> Grid.Cell msg
columnAll12 =
    Grid.cell
        [ Grid.size Grid.All 12
        ]


{-| Builds a ripple effect button used for completing some user input.

  - The button displays the specified label.
  - The button is disabled when the isValid indicator is false.
  - The specified msg is triggered when the button is clicked.

-}
completeButton : Material.Model -> (Material.Msg msg -> msg) -> String -> Bool -> msg -> Html msg
completeButton model mdl label isValid msg =
    Button.render mdl
        [ 0 ]
        model
        [ Button.ripple
        , if isValid then
            Button.colored
          else
            Button.disabled
        , Options.onClick msg
        ]
        [ text label ]


{-| Builds a ripple effect button used for cancelling some user input.

  - The button displays the specified label.
  - The button is disabled when the isValid indicator is false.
  - The specified msg is triggered when the button is clicked.

-}
cancelButton : Material.Model -> (Material.Msg msg -> msg) -> String -> msg -> Html msg
cancelButton model mdl label msg =
    Button.render mdl
        [ 0 ]
        model
        [ Button.ripple
        , Button.accent
        , Options.onClick msg
        ]
        [ Icon.i "chevron_left"
        , text label
        ]


{-| Builds a control bar with *ok* and *cancel* actions.
- The ok action is specified as button.
- The cancel action is specified as a msg to trigger on the cancel button
click.
-}
okCancelControlBar : Material.Model -> (Material.Msg msg -> msg) -> Html msg -> Html msg -> Html msg
okCancelControlBar model mdl okButton cancelButton =
    div [ class "control-bar" ]
        [ div [ class "control-bar__row" ]
            [ div [ class "control-bar__left-0" ]
                [ cancelButton ]
            , div [ class "control-bar__right-0" ]
                [ okButton ]
            ]
        ]


confirmDialog :
    { a | mdl : Material.Model }
    -> String
    -> (Material.Msg msg -> msg)
    -> msg
    -> Html msg
confirmDialog model title mdlAction action =
    Dialog.view
        []
        [ Dialog.title [] [ h4 [ class "mdl-dialog__title-text" ] [ text title ] ]
        , Dialog.content []
            [ p [] [ text "Are you sure?" ]
            ]
        , Dialog.actions []
            [ div [ class "control-bar" ]
                [ div [ class "control-bar__row" ]
                    [ div [ class "control-bar__left-0" ]
                        [ Button.render mdlAction
                            [ 1 ]
                            model.mdl
                            [ Dialog.closeOn "click"
                            , Button.accent
                            ]
                            [ text "Cancel" ]
                        ]
                    , div [ class "control-bar__right-0" ]
                        [ Button.render mdlAction
                            [ 0 ]
                            model.mdl
                            [ Dialog.closeOn "click"
                            , Button.colored
                            , Options.onClick action
                            ]
                            [ text "Confirm" ]
                        ]
                    ]
                ]
            ]
        ]


rhythm1SpacerDiv : Html msg
rhythm1SpacerDiv =
    div [ class "layout-spacer" ] []
