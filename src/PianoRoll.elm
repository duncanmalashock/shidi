module PianoRoll exposing (..)

import Html exposing (Html)
import Html.Attributes as Attr
import PianoRoll.Key


view : Html msg
view =
    Html.div
        [ Attr.class "piano-roll"
        , Attr.style "background-image" (backgroundImageAttr { height = 21 })
        ]
        []


backgroundImageAttr : { height : Int } -> String
backgroundImageAttr options =
    gridBackground
        |> String.replace "$keys" (PianoRoll.Key.view options.height)
        |> String.replace "$totalHeight" (String.fromInt (12 * options.height))
        |> String.replace "$midSplit" (String.fromInt (7 * options.height))
        |> String.replace "$lineColor" "#fff6"
        |> String.replace "\n" ""
        |> String.replace "#" "%23"
        |> wrapInUrl


wrapInUrl : String -> String
wrapInUrl input =
    "url('data:image/svg+xml," ++ input ++ "')"


gridBackground : String
gridBackground =
    """
<svg class="piano-roll__bg" xmlns="http://www.w3.org/2000/svg" viewBox="0 0 100 $totalHeight" width="100">
  <g class="piano-roll__keys">
    $keys
  </g>
  <g class="piano-roll__lines">
    <line x1="0" y1="0" x2="0" y2="300" stroke="$lineColor" />
    <line x1="25" y1="0" x2="25" y2="300" stroke="$lineColor" />
    <line x1="50" y1="0" x2="50" y2="300" stroke="$lineColor" />
    <line x1="75" y1="0" x2="75" y2="300" stroke="$lineColor" />
    <line x1="0" y1="0" x2="100" y2="0" stroke="$lineColor" />
    <line x1="0" y1="$midSplit" x2="100" y2="$midSplit" stroke="$lineColor" />
  </g>
</svg>
    """
