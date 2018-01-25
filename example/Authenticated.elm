module Authenticated exposing (authenticatedView)

import Material
import Auth
import Html exposing (Html, div, text, h4, img, form)
import Html.Lazy
import Html.Attributes exposing (title, class, href, src, action)
import Material.Button as Button
import Material.Icon as Icon
import Material.Textfield as Textfield
import Material.Options as Options
import ViewUtils


authenticatedView : Html msg
authenticatedView =
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
                        [ text "User Details"
                        ]
                    , div [ class "mdl-card__actions" ]
                        [-- div [ class "control-bar" ]
                         --     [ div [ class "control-bar__row" ]
                         --         [ div [ class "control-bar__left-0" ]
                         --             [ Button.render Mdl
                         --                 [ 2, 1 ]
                         --                 model.mdl
                         --                 [ Button.colored
                         --                 , Options.onClick TryAgain
                         --                 ]
                         --                 [ Icon.i "chevron_left"
                         --                 , text "Try Again"
                         --                 ]
                         --             ]
                         --         ]
                         --     ]
                        ]
                    ]
                ]
            ]
        ]
