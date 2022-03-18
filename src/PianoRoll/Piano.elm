module PianoRoll.Piano exposing (view)

import Html exposing (Html)
import Svg
import Svg.Attributes as Attr
import Svg.Events as Event


keysInOctave : Int
keysInOctave =
    12


whiteKeyHeight : Int
whiteKeyHeight =
    octaveHeight // whiteKeysInOctave


whiteKeyWidth : Int
whiteKeyWidth =
    55


blackKeyHeight : Int
blackKeyHeight =
    21


blackKeyWidth : Int
blackKeyWidth =
    25


octaveHeight : Int
octaveHeight =
    keysInOctave * blackKeyHeight


whiteKeysInOctave : Int
whiteKeysInOctave =
    7


cKeyLabelRightOffset : Int
cKeyLabelRightOffset =
    24


cKeyLabelBottomOffset : Int
cKeyLabelBottomOffset =
    12


view : (Int -> msg) -> Html msg
view onClick =
    List.range 0 9
        |> List.reverse
        |> List.map (viewOctave onClick)
        |> Html.div []


viewOctave : (Int -> msg) -> Int -> Svg.Svg msg
viewOctave onNoteClicked octave =
    let
        viewBoxAttr : String
        viewBoxAttr =
            [ 0
            , 0
            , whiteKeyWidth
            , octaveHeight
            ]
                |> List.map String.fromInt
                |> String.join " "

        semitoneOffset =
            (octave + 1) * 12
    in
    Svg.svg
        [ Attr.viewBox viewBoxAttr
        , Attr.width (String.fromInt whiteKeyWidth)
        , Attr.class "piano"
        ]
        [ Svg.g []
            ([ 11, 9, 7, 5, 4, 2, 0 ]
                |> List.indexedMap
                    (\heightIndex noteNumber ->
                        viewWhiteKey
                            (onNoteClicked (noteNumber + semitoneOffset))
                            heightIndex
                    )
            )
        , Svg.g []
            ([ 10, 8, 6 ]
                |> List.indexedMap
                    (\heightIndex noteNumber ->
                        viewBlackKey
                            (onNoteClicked (noteNumber + semitoneOffset))
                            heightIndex
                            0
                            blackKeyHeight
                    )
            )
        , Svg.g []
            ([ 3, 1 ]
                |> List.indexedMap
                    (\heightIndex noteNumber ->
                        viewBlackKey
                            (onNoteClicked (noteNumber + semitoneOffset))
                            heightIndex
                            4
                            0
                    )
            )
        , viewCKeyLabel octave
        ]


viewCKeyLabel : Int -> Svg.Svg msg
viewCKeyLabel octave =
    Svg.text_
        [ Attr.x <| String.fromInt (whiteKeyWidth - cKeyLabelRightOffset)
        , Attr.y <| String.fromInt (octaveHeight - cKeyLabelBottomOffset)
        , Attr.fill "black"
        , Attr.class "piano__text"
        ]
        [ Svg.text <| "C" ++ String.fromInt octave ]


viewWhiteKey : msg -> Int -> Svg.Svg msg
viewWhiteKey onNoteClicked index =
    let
        yValue : String
        yValue =
            index
                * whiteKeyHeight
                |> String.fromInt
    in
    Svg.rect
        [ Attr.fill "white"
        , Attr.x "0"
        , Attr.y yValue
        , Attr.width (String.fromInt whiteKeyWidth)
        , Attr.height (String.fromInt whiteKeyHeight)
        , Attr.stroke "black"
        , Attr.strokeWidth "3"
        , Attr.class "piano__key piano__key--white"
        , Event.onClick onNoteClicked
        ]
        []


viewBlackKey : msg -> Int -> Int -> Int -> Svg.Svg msg
viewBlackKey onNoteClicked baseOffset index otherOffset =
    let
        yValue : String
        yValue =
            (index + baseOffset)
                * (blackKeyHeight * 2)
                + otherOffset
                |> String.fromInt
    in
    Svg.rect
        [ Attr.fill "black"
        , Attr.x "0"
        , Attr.y yValue
        , Attr.width (String.fromInt blackKeyWidth)
        , Attr.height (String.fromInt blackKeyHeight)
        , Attr.stroke "black"
        , Attr.strokeWidth "3"
        , Attr.class "piano__key piano__key--black"
        , Event.onClick onNoteClicked
        ]
        []
