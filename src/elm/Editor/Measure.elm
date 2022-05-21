module Editor.Measure exposing (view)

import Editor.Coordinate
import Editor.Key
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Json.Decode
import Music
import Music.Meter
import Zoom


view :
    { zoom : Zoom.Zoom
    , onMovedMouseOverGrid : Editor.Coordinate.Music -> msg
    , onClickedLeftMouseButton : Editor.Coordinate.Music -> msg
    , onClickedRightMouseButton : Editor.Coordinate.Music -> msg
    , onMovedMouseAway : msg
    }
    -> Music.Measure
    -> Html msg
view options measure =
    let
        beatsPerMeasure : Int
        beatsPerMeasure =
            Music.Meter.beatsPerMeasure measure.meter

        cellsPerBeat : Int
        cellsPerBeat =
            2

        divisions : Int
        divisions =
            beatsPerMeasure * cellsPerBeat

        width : Int
        width =
            Zoom.cellSizeX options.zoom * divisions
    in
    Html.div
        ([ Html.Attributes.class "piano-roll__measure"
         , Html.Attributes.style "width" (String.fromInt width ++ "px")
         , Html.Attributes.style "background-image"
            (backgroundImageAttr
                { width = width
                , height = Zoom.cellSizeY options.zoom
                , divisions = divisions
                }
            )
         , Html.Attributes.style "background-repeat" "repeat-y"
         ]
            ++ mouseEvents
                { onMovedMouseOverGrid = options.onMovedMouseOverGrid
                , onClickedLeftMouseButton = options.onClickedLeftMouseButton
                , onClickedRightMouseButton = options.onClickedRightMouseButton
                , onMovedMouseAway = options.onMovedMouseAway
                , zoom = options.zoom
                , measure = measure
                }
        )
        []


mouseEvents :
    { onMovedMouseOverGrid : Editor.Coordinate.Music -> msg
    , onClickedLeftMouseButton : Editor.Coordinate.Music -> msg
    , onClickedRightMouseButton : Editor.Coordinate.Music -> msg
    , onMovedMouseAway : msg
    , zoom : Zoom.Zoom
    , measure : Music.Measure
    }
    -> List (Html.Attribute msg)
mouseEvents options =
    let
        onMouseMove : Html.Attribute msg
        onMouseMove =
            Html.Events.on "mousemove"
                (offsetDecoder
                    |> Json.Decode.map
                        (Editor.Coordinate.fromPixelsToMusic
                            options.measure
                            options.zoom
                        )
                    |> Json.Decode.map options.onMovedMouseOverGrid
                )

        onMouseUp : Html.Attribute msg
        onMouseUp =
            Html.Events.on "mouseup"
                (Json.Decode.andThen
                    (\mouseButton ->
                        if mouseButton == 0 then
                            offsetDecoder
                                |> Json.Decode.map
                                    (Editor.Coordinate.fromPixelsToMusic
                                        options.measure
                                        options.zoom
                                    )
                                |> Json.Decode.map options.onClickedLeftMouseButton

                        else if mouseButton == 2 then
                            offsetDecoder
                                |> Json.Decode.map
                                    (Editor.Coordinate.fromPixelsToMusic
                                        options.measure
                                        options.zoom
                                    )
                                |> Json.Decode.map options.onClickedRightMouseButton

                        else
                            Json.Decode.fail "bad witch club"
                    )
                    (Json.Decode.field "button" Json.Decode.int)
                )

        offsetDecoder : Json.Decode.Decoder Editor.Coordinate.Pixel
        offsetDecoder =
            Json.Decode.map2
                Editor.Coordinate.Pixel
                (Json.Decode.field "offsetX" Json.Decode.int)
                (Json.Decode.field "offsetY" Json.Decode.int)
    in
    [ onMouseMove
    , onMouseUp
    , Html.Events.onMouseLeave options.onMovedMouseAway
    ]


backgroundImageAttr : { height : Int, width : Int, divisions : Int } -> String
backgroundImageAttr options =
    gridBackground
        |> String.replace "$keys" (Editor.Key.view options.width options.height)
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
                "#ffffff4A"

            else if modBy 2 index == 0 then
                "#ffffff30"

            else
                "#ffffff20"
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
