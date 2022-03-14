module PianoRoll exposing (view)

import Coordinate
import Html exposing (Html)
import Html.Attributes as Attr
import Html.Events
import Json.Decode
import Music
import PianoRoll.Key
import Project


view :
    { onMouseMove : Coordinate.Music -> msg
    , onMouseLeave : msg
    , onLeftClick : Coordinate.Music -> msg
    , onRightClick : Coordinate.Music -> msg
    , project : Project.Project
    , mousePosition : Maybe Coordinate.Music
    }
    -> Html msg
view options =
    Html.div
        [ Attr.class "piano-roll__wrapper" ]
        [ viewRoll options
        , viewNotes options.project
        , case options.mousePosition of
            Just coordinate ->
                viewNote "mediumseagreen"
                    (Coordinate.fromMusicToNoteEvent coordinate)

            Nothing ->
                Html.text ""
        ]


viewRoll :
    { a
        | onMouseMove : Coordinate.Music -> msg
        , onMouseLeave : msg
        , onLeftClick : Coordinate.Music -> msg
        , onRightClick : Coordinate.Music -> msg
    }
    -> Html msg
viewRoll options =
    let
        mouseEventAttributes : List (Html.Attribute msg)
        mouseEventAttributes =
            mouseEvents
                { onMouseMove = options.onMouseMove
                , onMouseLeave = options.onMouseLeave
                , onLeftClick = options.onLeftClick
                , onRightClick = options.onRightClick
                }
    in
    Html.div
        ([ Attr.class "piano-roll"
         , Attr.style "background-image" (backgroundImageAttr { height = 21 })
         ]
            ++ mouseEventAttributes
        )
        []


viewNotes : Project.Project -> Html msg
viewNotes project =
    Project.noteEvents project
        |> List.map (viewNote "deeppink")
        |> Html.div []


viewNote : String -> Music.NoteEvent -> Html msg
viewNote color noteEvent =
    let
        { x, y } =
            noteEvent
                |> Coordinate.fromNoteEventToMusic
                |> Coordinate.fromMusicToPixels
                |> Coordinate.pixelsXY
    in
    Html.div
        [ Attr.class "note-preview"
        , Attr.style "background" color
        , Attr.style "transform"
            ("translate($x, $y)"
                |> String.replace "$x" (String.fromInt x ++ "px")
                |> String.replace "$y" (String.fromInt y ++ "px")
            )
        ]
        []


mouseEvents :
    { onMouseMove : Coordinate.Music -> msg
    , onMouseLeave : msg
    , onLeftClick : Coordinate.Music -> msg
    , onRightClick : Coordinate.Music -> msg
    }
    -> List (Html.Attribute msg)
mouseEvents options =
    let
        onMouseMove : Html.Attribute msg
        onMouseMove =
            Html.Events.on "mousemove"
                (offsetDecoder
                    |> Json.Decode.map Coordinate.fromPixelsToMusic
                    |> Json.Decode.map options.onMouseMove
                )

        onMouseUp : Html.Attribute msg
        onMouseUp =
            Html.Events.on "mouseup"
                (Json.Decode.andThen
                    (\mouseButton ->
                        if mouseButton == 0 then
                            offsetDecoder
                                |> Json.Decode.map Coordinate.fromPixelsToMusic
                                |> Json.Decode.map options.onLeftClick

                        else if mouseButton == 2 then
                            offsetDecoder
                                |> Json.Decode.map Coordinate.fromPixelsToMusic
                                |> Json.Decode.map options.onRightClick

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
    in
    [ onMouseMove
    , onMouseUp
    , Html.Events.onMouseLeave options.onMouseLeave
    ]


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
