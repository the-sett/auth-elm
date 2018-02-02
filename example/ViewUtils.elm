module ViewUtils exposing (rhythm1SpacerDiv)

import Html exposing (Html, div)
import Html.Attributes exposing (class)


rhythm1SpacerDiv : Html msg
rhythm1SpacerDiv =
    div [ class "layout-spacer" ] []
