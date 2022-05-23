module Editor.Measure exposing (view, widthInPixels)

import Editor.Coordinate
import Editor.Measure.Background
import Editor.Measure.GridLine
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Json.Decode
import List.Extra
import Music
import Music.Duration
import Music.Meter
import Zoom


widthInPixels : Music.Measure -> Zoom.Zoom -> Int
widthInPixels measure zoom =
    Zoom.cellSizeX zoom * eighthNotesInMeasure measure.meter


eighthNotesInMeasure : Music.Meter.Meter -> Int
eighthNotesInMeasure meter =
    subdivisionsInMeasure Music.Duration.eighth meter


sixteenthNotesInMeasure : Music.Meter.Meter -> Int
sixteenthNotesInMeasure meter =
    subdivisionsInMeasure Music.Duration.sixteenth meter


subdivisionsInMeasure : Music.Duration.Duration -> Music.Meter.Meter -> Int
subdivisionsInMeasure divideBy meter =
    let
        beatsPerMeasure : Int
        beatsPerMeasure =
            Music.Meter.beatsPerMeasure meter

        beatDuration : Music.Duration.Duration
        beatDuration =
            Music.Meter.beatDuration meter
    in
    Music.Duration.divide beatDuration divideBy
        |> Music.Duration.toFloat
        |> round
        |> (*) beatsPerMeasure


type LineGrouping
    = GroupedByQuarters
    | GroupedByThreeEighths


lineGrouping : Music.Meter.Meter -> Maybe LineGrouping
lineGrouping meter =
    let
        beatsPerMeasure : Int
        beatsPerMeasure =
            Music.Meter.beatsPerMeasure meter

        beatDuration : Music.Duration.Duration
        beatDuration =
            Music.Meter.beatDuration meter
    in
    if
        (beatDuration == Music.Duration.quarter)
            || (beatDuration == Music.Duration.half)
    then
        Just GroupedByQuarters

    else if beatDuration == Music.Duration.eighth then
        if modBy 3 beatsPerMeasure == 0 then
            Just GroupedByThreeEighths

        else
            Nothing

    else
        Nothing


primaryGridLinesIndicesInEights : Music.Meter.Meter -> List Int
primaryGridLinesIndicesInEights meter =
    let
        indices : List Int
        indices =
            List.repeat (eighthNotesInMeasure meter) 0
                |> List.indexedMap (\i _ -> i)
    in
    case lineGrouping meter of
        Just GroupedByQuarters ->
            indices
                |> List.filter (\i -> modBy 2 i == 0)

        Just GroupedByThreeEighths ->
            indices
                |> List.filter (\i -> modBy 3 i == 0)

        Nothing ->
            []


gridLines :
    Zoom.Zoom
    -> Music.Meter.Meter
    -> List Editor.Measure.GridLine.GridLine
gridLines zoom meter =
    let
        showSixteenths : Bool
        showSixteenths =
            Zoom.cellSizeX zoom >= 58

        linesBeforeReplacement : List Editor.Measure.GridLine.GridLine
        linesBeforeReplacement =
            if showSixteenths then
                Editor.Measure.GridLine.tertiary
                    |> List.repeat (sixteenthNotesInMeasure meter)

            else
                Editor.Measure.GridLine.secondary
                    |> List.repeat (eighthNotesInMeasure meter)

        setInitial :
            List Editor.Measure.GridLine.GridLine
            -> List Editor.Measure.GridLine.GridLine
        setInitial list =
            updateGridLinesAtIndices
                [ 0 ]
                Editor.Measure.GridLine.initial
                list

        setSecondaries :
            List Editor.Measure.GridLine.GridLine
            -> List Editor.Measure.GridLine.GridLine
        setSecondaries list =
            let
                indices : List Int
                indices =
                    if showSixteenths then
                        List.repeat (eighthNotesInMeasure meter) 0
                            |> List.indexedMap (\i _ -> i * 2)

                    else
                        List.repeat (eighthNotesInMeasure meter) 0
                            |> List.indexedMap (\i _ -> i)
            in
            updateGridLinesAtIndices
                indices
                Editor.Measure.GridLine.secondary
                list

        setPrimaries :
            List Editor.Measure.GridLine.GridLine
            -> List Editor.Measure.GridLine.GridLine
        setPrimaries list =
            let
                indices : List Int
                indices =
                    if showSixteenths then
                        primaryGridLinesIndicesInEights meter
                            |> List.map (\n -> n * 2)

                    else
                        primaryGridLinesIndicesInEights meter
            in
            updateGridLinesAtIndices
                indices
                Editor.Measure.GridLine.primary
                list
    in
    linesBeforeReplacement
        |> setSecondaries
        |> setPrimaries
        |> setInitial


updateGridLinesAtIndices :
    List Int
    -> Editor.Measure.GridLine.GridLine
    -> List Editor.Measure.GridLine.GridLine
    -> List Editor.Measure.GridLine.GridLine
updateGridLinesAtIndices indices gridLineType list =
    List.foldl
        (\index -> List.Extra.updateAt index (always gridLineType))
        list
        indices


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
        width : Int
        width =
            widthInPixels measure options.zoom
    in
    Html.div
        ([ Html.Attributes.class "piano-roll__measure"
         , Html.Attributes.style "width"
            (String.fromInt width ++ "px")
         , Editor.Measure.Background.attribute
            { width = width
            , height = Zoom.cellSizeY options.zoom
            , gridLines = gridLines options.zoom measure.meter
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
