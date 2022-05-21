module Editor.Measure exposing (view)

import Editor.Coordinate
import Editor.Measure.Background
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
         , Editor.Measure.Background.attribute
            { width = width
            , height = Zoom.cellSizeY options.zoom
            , divisions = divisions
            , zoom = options.zoom
            }
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
