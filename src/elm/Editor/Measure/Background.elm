module Editor.Measure.Background exposing (attribute)

import Editor.Measure.Key
import Html
import Html.Attributes
import Zoom


attribute :
    { height : Int, width : Int, divisions : Int, zoom : Zoom.Zoom }
    -> Html.Attribute msg
attribute options =
    Html.Attributes.style "background-image"
        (backgroundImageAttr
            { width = options.width
            , height = Zoom.cellSizeY options.zoom
            , divisions = options.divisions
            }
        )


backgroundImageAttr : { height : Int, width : Int, divisions : Int } -> String
backgroundImageAttr options =
    gridBackground
        |> String.replace "$keys" (Editor.Measure.Key.view options.width options.height)
        |> String.replace "$verticalLines"
            (List.range 0 (options.divisions - 1)
                |> List.map (viewVerticalLine options)
                |> String.join ""
            )
        |> String.replace "$totalHeight" (String.fromInt (12 * options.height))
        |> String.replace "$midSplit" (String.fromInt (7 * options.height))
        |> String.replace "$horizontalLineColor" "#fff1"
        |> String.replace "$width" (String.fromInt options.width)
        |> String.replace "\n" ""
        |> String.replace "#" "%23"
        |> wrapInUrl


wrapInUrl : String -> String
wrapInUrl input =
    "url('data:image/svg+xml," ++ input ++ "')"


viewVerticalLine : { height : Int, width : Int, divisions : Int } -> Int -> String
viewVerticalLine { width, height, divisions } index =
    let
        positionBeforeOffset : Int
        positionBeforeOffset =
            (toFloat width / toFloat divisions)
                * toFloat index
                |> round

        position : Int
        position =
            if index == 0 then
                -- Because SVG line thickness is centered on the stroke, nudge
                -- by one pixel at the boundary to avoid half of the first
                -- vertical line from being clipped off.
                1

            else
                positionBeforeOffset

        color : String
        color =
            if index == 0 then
                "#ffffff40"

            else if modBy 2 index == 0 then
                "#ffffff30"

            else
                "#ffffff18"
    in
    """<line x1="$x" y1="0" x2="$x" y2="$totalHeight" stroke="$verticalLineColor" />"""
        |> String.replace "$x" (String.fromInt position)
        |> String.replace "$totalHeight" (String.fromInt (12 * height))
        |> String.replace "$verticalLineColor" color


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
