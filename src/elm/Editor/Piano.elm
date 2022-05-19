module Editor.Piano exposing (view)

import Html exposing (Html)
import Html.Attributes
import Svg
import Svg.Attributes as Attr
import Svg.Events as Event


view : Int -> (Int -> msg) -> Html msg
view height onClick =
    let
        viewOctaves : List (Html.Html msg)
        viewOctaves =
            List.range 0 9
                |> List.reverse
                |> List.map
                    (viewOctave
                        { onNoteClicked = onClick
                        , height = height
                        }
                    )
    in
    Html.div
        [ Html.Attributes.class "piano"
        ]
        viewOctaves


viewOctave :
    { onNoteClicked : Int -> msg
    , height : Int
    }
    -> Int
    -> Svg.Svg msg
viewOctave { onNoteClicked, height } octave =
    let
        keysInOctave : Int
        keysInOctave =
            12

        whiteKeyHeight : Int
        whiteKeyHeight =
            octaveHeight // whiteKeysInOctave

        whiteKeyWidth : Int
        whiteKeyWidth =
            37

        xOffset : Int
        xOffset =
            10

        blackKeyHeight : Int
        blackKeyHeight =
            height

        blackKeyWidth : Int
        blackKeyWidth =
            18

        keyRoundedCorner : Int
        keyRoundedCorner =
            2

        octaveHeight : Int
        octaveHeight =
            keysInOctave * blackKeyHeight

        whiteKeysInOctave : Int
        whiteKeysInOctave =
            7

        cKeyLabelRightOffset : Int
        cKeyLabelRightOffset =
            17

        cKeyLabelBottomOffset : Int
        cKeyLabelBottomOffset =
            ((height + 6) - fontSize) // 2

        fontSize : Int
        fontSize =
            11

        viewBoxAttr : String
        viewBoxAttr =
            [ 0
            , 0
            , whiteKeyWidth
            , octaveHeight
            ]
                |> List.map String.fromInt
                |> String.join " "

        semitoneOffset : Int
        semitoneOffset =
            (octave + 1) * 12

        viewCKeyLabel : Int -> Svg.Svg msg
        viewCKeyLabel octave_ =
            let
                styles : String
                styles =
                    [ "font-size: " ++ (String.fromInt fontSize ++ "px")
                    , "font-weight: bold"
                    , "font-family: sans-serif"
                    , "pointer-events: none"
                    ]
                        |> String.join "; "
            in
            Svg.text_
                [ Attr.x <| String.fromInt (whiteKeyWidth - cKeyLabelRightOffset)
                , Attr.y <| String.fromInt (octaveHeight - cKeyLabelBottomOffset)
                , Attr.fill "black"
                , Attr.class "piano__text"
                , Attr.style styles
                ]
                [ Svg.text <| "C" ++ String.fromInt octave_ ]

        viewWhiteKey : msg -> Int -> Svg.Svg msg
        viewWhiteKey onNoteClicked_ index =
            let
                yValue : String
                yValue =
                    index
                        * whiteKeyHeight
                        |> String.fromInt
            in
            Svg.rect
                [ Attr.fill "#eee"
                , Attr.x (String.fromInt -xOffset)
                , Attr.y yValue
                , Attr.width (String.fromInt (whiteKeyWidth + xOffset))
                , Attr.height (String.fromInt whiteKeyHeight)
                , Attr.stroke "#222"
                , Attr.strokeWidth "1"
                , Attr.rx (String.fromInt keyRoundedCorner)
                , Attr.class "piano__key piano__key--white"
                , Event.onClick onNoteClicked_
                ]
                []

        viewBlackKey : msg -> Int -> Int -> Int -> Svg.Svg msg
        viewBlackKey onNoteClicked_ baseOffset index otherOffset =
            let
                yValue : String
                yValue =
                    (index + baseOffset)
                        * (blackKeyHeight * 2)
                        + otherOffset
                        |> String.fromInt
            in
            Svg.rect
                [ Attr.fill "#222"
                , Attr.x (String.fromInt -xOffset)
                , Attr.y yValue
                , Attr.width (String.fromInt (blackKeyWidth + xOffset))
                , Attr.height (String.fromInt blackKeyHeight)
                , Attr.stroke "#222"
                , Attr.strokeWidth "1"
                , Attr.rx (String.fromInt keyRoundedCorner)
                , Attr.class "piano__key piano__key--black"
                , Event.onClick onNoteClicked_
                ]
                []
    in
    Svg.svg
        [ Attr.viewBox viewBoxAttr
        , Attr.width (String.fromInt whiteKeyWidth)
        , Attr.style "display: block"
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
