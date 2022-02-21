module Piano exposing (viewOctave, zoomDefault)

import Svg
import Svg.Attributes as Attr
import Svg.Events as Event


type Zoom
    = ZoomXXS
    | ZoomXS
    | ZoomS
    | ZoomM
    | ZoomL
    | ZoomXL
    | ZoomXXL


zoomDefault : Zoom
zoomDefault =
    ZoomL


whiteKeyHeight : Zoom -> Int
whiteKeyHeight zoom =
    case zoom of
        ZoomXXS ->
            45

        ZoomXS ->
            50

        ZoomS ->
            55

        ZoomM ->
            65

        ZoomL ->
            80

        ZoomXL ->
            110

        ZoomXXL ->
            160


whiteKeyWidth : Int
whiteKeyWidth =
    120


blackKeyHeight : Zoom -> Int
blackKeyHeight zoom =
    (whiteKeyHeight zoom * 2) // 3


blackKeyWidth : Int
blackKeyWidth =
    66


viewOctave : Zoom -> (Int -> msg) -> Int -> Svg.Svg msg
viewOctave zoom onNoteClicked octave =
    let
        viewBoxAttr : String
        viewBoxAttr =
            [ 0
            , 0
            , whiteKeyWidth
            , octaveHeight zoom
            ]
                |> List.map String.fromInt
                |> String.join " "

        semitoneOffset =
            octave * 12
    in
    Svg.svg
        [ Attr.viewBox viewBoxAttr
        , Attr.width "35"
        , Attr.class "piano"
        ]
        [ Svg.g []
            ([ ( 0, 11 )
             , ( 1, 9 )
             , ( 2, 7 )
             , ( 3, 5 )
             , ( 4, 4 )
             , ( 5, 2 )
             , ( 6, 0 )
             ]
                |> List.map
                    (\( heightIndex, noteNumber ) ->
                        viewWhiteKey zoom
                            (onNoteClicked (noteNumber + semitoneOffset))
                            heightIndex
                    )
            )
        , Svg.g []
            ([ ( 0, 10 )
             , ( 1, 8 )
             , ( 2, 6 )
             ]
                |> List.map
                    (\( heightIndex, noteNumber ) ->
                        viewBlackKey zoom
                            (onNoteClicked (noteNumber + semitoneOffset))
                            heightIndex
                            1
                    )
            )
        , Svg.g []
            ([ ( 0, 3 )
             , ( 1, 1 )
             ]
                |> List.map
                    (\( heightIndex, noteNumber ) ->
                        viewBlackKey zoom
                            (onNoteClicked (noteNumber + semitoneOffset))
                            heightIndex
                            5
                    )
            )
        , viewCKeyLabel zoom octave
        ]


viewCKeyLabel : Zoom -> Int -> Svg.Svg msg
viewCKeyLabel zoom octave =
    let
        cKeyLabelRightOffset : Int
        cKeyLabelRightOffset =
            46

        cKeyLabelBottomOffset : Int
        cKeyLabelBottomOffset =
            12
    in
    Svg.text_
        [ Attr.x <| String.fromInt (whiteKeyWidth - cKeyLabelRightOffset)
        , Attr.y <| String.fromInt (octaveHeight zoom - cKeyLabelBottomOffset)
        , Attr.fill "black"
        , Attr.class "piano__text"
        ]
        [ Svg.text <| "C" ++ String.fromInt octave ]


octaveHeight : Zoom -> Int
octaveHeight zoom =
    whiteKeyHeight zoom * whiteKeysInOctave


whiteKeysInOctave : Int
whiteKeysInOctave =
    7


viewWhiteKey : Zoom -> msg -> Int -> Svg.Svg msg
viewWhiteKey zoom onNoteClicked index =
    let
        yValue : String
        yValue =
            index
                * whiteKeyHeight zoom
                |> String.fromInt
    in
    Svg.rect
        [ Attr.fill "white"
        , Attr.x "0"
        , Attr.y yValue
        , Attr.width (String.fromInt whiteKeyWidth)
        , Attr.height (String.fromInt (whiteKeyHeight zoom))
        , Attr.stroke "black"
        , Attr.strokeWidth "3"
        , Attr.class "piano__key piano__key--white"
        , Event.onClick onNoteClicked
        ]
        []


viewBlackKey : Zoom -> msg -> Int -> Int -> Svg.Svg msg
viewBlackKey zoom onNoteClicked baseOffset index =
    let
        yValue : String
        yValue =
            (index + baseOffset)
                * whiteKeyHeight zoom
                - (blackKeyHeight zoom // 2)
                |> String.fromInt
    in
    Svg.rect
        [ Attr.fill "black"
        , Attr.x "0"
        , Attr.y yValue
        , Attr.width (String.fromInt blackKeyWidth)
        , Attr.height (String.fromInt (blackKeyHeight zoom))
        , Attr.stroke "black"
        , Attr.strokeWidth "3"
        , Attr.class "piano__key piano__key--black"
        , Event.onClick onNoteClicked
        ]
        []
