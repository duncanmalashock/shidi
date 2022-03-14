module PianoRoll exposing (view)

import Coordinate
import Html exposing (Html)
import Html.Attributes as Attr
import Html.Events
import Json.Decode
import PianoRoll.Key


view :
    { onMouseMove : Coordinate.Pixels -> msg
    , onMouseLeave : msg
    , onLeftClick : Coordinate.Pixels -> msg
    , onRightClick : Coordinate.Pixels -> msg
    }
    -> Html msg
view options =
    Html.div
        [ Attr.class "piano-roll"
        , Attr.style "background-image" (backgroundImageAttr { height = 21 })
        , Html.Events.on "mousemove" (offsetDecoder |> Json.Decode.map options.onMouseMove)
        , onMouseUp
            { onLeftClick = options.onLeftClick
            , onRightClick = options.onRightClick
            }
        , Html.Events.onMouseLeave options.onMouseLeave
        ]
        []


onMouseUp :
    { onLeftClick : Coordinate.Pixels -> msg
    , onRightClick : Coordinate.Pixels -> msg
    }
    -> Html.Attribute msg
onMouseUp { onLeftClick, onRightClick } =
    Html.Events.on "mouseup"
        (Json.Decode.andThen
            (\mouseButton ->
                if mouseButton == 0 then
                    offsetDecoder |> Json.Decode.map onLeftClick

                else if mouseButton == 2 then
                    offsetDecoder |> Json.Decode.map onRightClick

                else
                    Json.Decode.fail "bad witch club 🧙\u{200D}♀️"
            )
            (Json.Decode.field "button" Json.Decode.int)
        )


offsetDecoder : Json.Decode.Decoder Coordinate.Pixels
offsetDecoder =
    Json.Decode.map2
        Coordinate.pixels
        (Json.Decode.field "offsetX" Json.Decode.int)
        (Json.Decode.field "offsetY" Json.Decode.int)


backgroundImageAttr : { height : Int } -> String
backgroundImageAttr options =
    gridBackground
        |> String.replace "$keys" (PianoRoll.Key.view options.height)
        |> String.replace "$verticalLines"
            (List.range 0 4
                |> List.map (viewVerticalLine options.height)
                |> String.join ""
            )
        |> String.replace "$totalHeight" (String.fromInt (12 * options.height))
        |> String.replace "$midSplit" (String.fromInt (7 * options.height))
        |> String.replace "$verticalLineColor" "#fff5"
        |> String.replace "$horizontalLineColor" "#fff2"
        |> String.replace "$width" (String.fromInt (4 * options.height))
        |> String.replace "\n" ""
        |> String.replace "#" "%23"
        |> wrapInUrl


wrapInUrl : String -> String
wrapInUrl input =
    "url('data:image/svg+xml," ++ input ++ "')"


viewVerticalLine : Int -> Int -> String
viewVerticalLine height index =
    """<line x1="$x" y1="0" x2="$x" y2="$totalHeight" stroke="$verticalLineColor" />"""
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
    <line x1="0" y1="0" x2="$width" y2="0" stroke="$horizontalLineColor" />
    <line x1="0" y1="$midSplit" x2="$width" y2="$midSplit" stroke="$horizontalLineColor" />
    <line x1="0" y1="$totalHeight" x2="$width" y2="$totalHeight" stroke="$horizontalLineColor" />
  </g>
</svg>
    """
