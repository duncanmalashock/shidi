module Editor.Measure exposing (..)

import Editor.Coordinate
import Editor.Key
import Editor.Scale
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Json.Decode
import Music


viewMeasure :
    { scaleX : Editor.Scale.ScaleX
    , scaleY : Editor.Scale.ScaleY
    , onMovedMouseOverGrid : Editor.Coordinate.MusicCoordinate -> msg
    , onClickedLeftMouseButton : Editor.Coordinate.MusicCoordinate -> msg
    , onClickedRightMouseButton : Editor.Coordinate.MusicCoordinate -> msg
    , onMovedMouseAway : msg
    }
    -> Music.Measure
    -> Html msg
viewMeasure options measure =
    Html.div
        ([ Html.Attributes.class "piano-roll__measure"
         , Html.Attributes.style "width" "calc(8 * 21px)"
         , Html.Attributes.style "background-image"
            (backgroundImageAttr
                { width = Editor.Scale.cellSizeX options.scaleX
                , height = Editor.Scale.cellSizeY options.scaleY
                }
            )
         ]
            ++ mouseEvents
                { onMovedMouseOverGrid = options.onMovedMouseOverGrid
                , onClickedLeftMouseButton = options.onClickedLeftMouseButton
                , onClickedRightMouseButton = options.onClickedRightMouseButton
                , onMovedMouseAway = options.onMovedMouseAway
                , scaleX = options.scaleX
                , scaleY = options.scaleY
                , measure = measure
                }
        )
        []


mouseEvents :
    { onMovedMouseOverGrid : Editor.Coordinate.MusicCoordinate -> msg
    , onClickedLeftMouseButton : Editor.Coordinate.MusicCoordinate -> msg
    , onClickedRightMouseButton : Editor.Coordinate.MusicCoordinate -> msg
    , onMovedMouseAway : msg
    , scaleX : Editor.Scale.ScaleX
    , scaleY : Editor.Scale.ScaleY
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
                            options.scaleX
                            options.scaleY
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
                                        options.scaleX
                                        options.scaleY
                                    )
                                |> Json.Decode.map options.onClickedLeftMouseButton

                        else if mouseButton == 2 then
                            offsetDecoder
                                |> Json.Decode.map
                                    (Editor.Coordinate.fromPixelsToMusic
                                        options.measure
                                        options.scaleX
                                        options.scaleY
                                    )
                                |> Json.Decode.map options.onClickedRightMouseButton

                        else
                            Json.Decode.fail "bad witch club"
                    )
                    (Json.Decode.field "button" Json.Decode.int)
                )

        offsetDecoder : Json.Decode.Decoder Editor.Coordinate.PixelCoordinate
        offsetDecoder =
            Json.Decode.map2
                Editor.Coordinate.PixelCoordinate
                (Json.Decode.field "offsetX" Json.Decode.int)
                (Json.Decode.field "offsetY" Json.Decode.int)
    in
    [ onMouseMove
    , onMouseUp
    , Html.Events.onMouseLeave options.onMovedMouseAway
    ]


backgroundImageAttr : { height : Int, width : Int } -> String
backgroundImageAttr options =
    gridBackground
        |> String.replace "$keys" (Editor.Key.view options.height)
        |> String.replace "$verticalLines"
            (List.range 0 4
                |> List.map (viewVerticalLine options)
                |> String.join ""
            )
        |> String.replace "$totalHeight" (String.fromInt (12 * options.height))
        |> String.replace "$midSplit" (String.fromInt (7 * options.height))
        |> String.replace "$verticalLineColor" "#fff2"
        |> String.replace "$horizontalLineColor" "#fff1"
        |> String.replace "$width" (String.fromInt options.width)
        |> String.replace "\n" ""
        |> String.replace "#" "%23"
        |> wrapInUrl


wrapInUrl : String -> String
wrapInUrl input =
    "url('data:image/svg+xml," ++ input ++ "')"


viewVerticalLine : { height : Int, width : Int } -> Int -> String
viewVerticalLine { width, height } index =
    """<line x1="$x" y1="0" x2="$x" y2="$totalHeight" stroke="$verticalLineColor" />"""
        |> String.replace "$x" (String.fromInt (width * index))
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
