module Stories.Button exposing (main)

import Html exposing (Html)
import Storybook.Component exposing (Component)
import Storybook.Controls
import Ui.Button


main : Component () Msg
main =
    Storybook.Component.stateless
        { controls = Storybook.Controls.none
        , view = view
        }


type Msg
    = UserClickedButton


view : () -> Html Msg
view controls =
    Ui.Button.view
        { label = "Click me!"
        , onClick = UserClickedButton
        }
