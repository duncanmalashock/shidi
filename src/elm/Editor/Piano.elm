module Editor.Piano exposing (view)

import Html exposing (Html)
import Html.Attributes
import Svg
import Svg.Attributes
import Svg.Events


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
            if whiteKeyHeight >= 14 then
                11

            else
                9

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
                [ Svg.Attributes.x <| String.fromInt (whiteKeyWidth - cKeyLabelRightOffset)
                , Svg.Attributes.y <| String.fromInt (octaveHeight - cKeyLabelBottomOffset)
                , Svg.Attributes.fill "black"
                , Svg.Attributes.class "piano__text"
                , Svg.Attributes.style styles
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
                [ Svg.Attributes.fill "#eee"
                , Svg.Attributes.x (String.fromInt -xOffset)
                , Svg.Attributes.y yValue
                , Svg.Attributes.width (String.fromInt (whiteKeyWidth + xOffset))
                , Svg.Attributes.height (String.fromInt whiteKeyHeight)
                , Svg.Attributes.stroke "#222"
                , Svg.Attributes.strokeWidth "1"
                , Svg.Attributes.rx (String.fromInt keyRoundedCorner)
                , Svg.Attributes.class "piano__key piano__key--white"
                , Svg.Events.onClick onNoteClicked_
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
                [ Svg.Attributes.fill "#222"
                , Svg.Attributes.x (String.fromInt -xOffset)
                , Svg.Attributes.y yValue
                , Svg.Attributes.width (String.fromInt (blackKeyWidth + xOffset))
                , Svg.Attributes.height (String.fromInt blackKeyHeight)
                , Svg.Attributes.stroke "#222"
                , Svg.Attributes.strokeWidth "1"
                , Svg.Attributes.rx (String.fromInt keyRoundedCorner)
                , Svg.Attributes.class "piano__key piano__key--black"
                , Svg.Events.onClick onNoteClicked_
                ]
                []
    in
    Svg.svg
        [ Svg.Attributes.viewBox viewBoxAttr
        , Svg.Attributes.width (String.fromInt whiteKeyWidth)
        , Svg.Attributes.style "display: block"
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
