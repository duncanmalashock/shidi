module Ui.Button exposing (view)

import Html exposing (Html)
import Html.Events


view : { label : String, onClick : msg } -> Html msg
view options =
    Html.button
        [ Html.Events.onClick options.onClick ]
        [ Html.text options.label ]
