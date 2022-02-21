module PianoRoll exposing (..)

import Html exposing (Html)
import Html.Attributes as Attr
import Html.Events
import Json.Decode
import PianoRoll.Key


view :
    { onMouseMove : { x : Int, y : Int } -> msg
    , onMouseLeave : msg
    }
    -> Html msg
view options =
    Html.div
        [ Attr.class "piano-roll"
        , Attr.style "background-image" (backgroundImageAttr { height = 21 })
        , Html.Events.on "mousemove" (mouseMoveDecoder options.onMouseMove)
        , Html.Events.onMouseLeave options.onMouseLeave
        ]
        []


mouseMoveDecoder : ({ x : Int, y : Int } -> msg) -> Json.Decode.Decoder msg
mouseMoveDecoder onMouseMove =
    Json.Decode.map2
        (\x y -> onMouseMove { x = x, y = y })
        (Json.Decode.field "offsetX" Json.Decode.int)
        (Json.Decode.field "offsetY" Json.Decode.int)


backgroundImageAttr : { height : Int } -> String
backgroundImageAttr options =
    gridBackground
        |> String.replace "$keys" (PianoRoll.Key.view options.height)
        |> String.replace "$verticalLines"
            (List.range 0 3
                |> List.map (viewVerticalLine options.height)
                |> String.join ""
            )
        |> String.replace "$totalHeight" (String.fromInt (12 * options.height))
        |> String.replace "$midSplit" (String.fromInt (7 * options.height))
        |> String.replace "$lineColor" "#fff6"
        |> String.replace "$width" (String.fromInt (4 * options.height))
        |> String.replace "\n" ""
        |> String.replace "#" "%23"
        |> wrapInUrl


wrapInUrl : String -> String
wrapInUrl input =
    "url('data:image/svg+xml," ++ input ++ "')"


viewVerticalLine : Int -> Int -> String
viewVerticalLine height index =
    """<line x1="$x" y1="0" x2="$x" y2="$totalHeight" stroke="$lineColor" />"""
        |> String.replace "$x" (String.fromInt (height * index))
        |> String.replace "$totalHeight" (String.fromInt (12 * height))


gridBackground : String
gridBackground =
    """
<svg class="piano-roll__bg" xmlns="http://www.w3.org/2000/svg" viewBox="0 0 $width $totalHeight" width="$width">
  <g class="piano-roll__keys">
    $keys
  </g>
  <g class="piano-roll__lines">
    $verticalLines
    <line x1="0" y1="0" x2="$width" y2="0" stroke="$lineColor" />
    <line x1="0" y1="$midSplit" x2="100" y2="$midSplit" stroke="$lineColor" />
  </g>
</svg>
    """
