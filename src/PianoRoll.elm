module PianoRoll exposing
    ( Model, init
    , Msg, update, OutMsg(..)
    , view
    )

{-|

@docs Model, init

@docs Msg, update, OutMsg

@docs view

-}

import Html exposing (Html)
import Html.Attributes
import Html.Events
import Json.Decode
import MidiEvent
import Music
import Music.Duration
import Music.Note
import Music.Pitch
import PianoRoll.Key
import Project


cellSizeX : Int
cellSizeX =
    21


cellSizeY : Int
cellSizeY =
    21


type Model
    = Model
        { mousePosition : Maybe MusicCoordinate
        }


init : Model
init =
    Model
        { mousePosition = Nothing
        }


type Msg
    = UserMovedMouseOverGrid MusicCoordinate
    | UserMovedMouseAway
    | UserClickedLeftMouseButton MusicCoordinate
    | UserClickedRightMouseButton MusicCoordinate


type OutMsg
    = AddNoteEvent Music.NoteEvent Int
    | RemoveNoteEvent Music.NoteEvent


update : Msg -> Model -> ( Model, Maybe OutMsg )
update msg (Model model) =
    case msg of
        UserMovedMouseOverGrid newPosition ->
            ( Model
                { model
                    | mousePosition = Just newPosition
                }
            , Nothing
            )

        UserMovedMouseAway ->
            ( Model
                { model
                    | mousePosition = Nothing
                }
            , Nothing
            )

        UserClickedLeftMouseButton coordinate ->
            let
                noteEvent : Music.NoteEvent
                noteEvent =
                    fromMusicToNoteEvent coordinate
            in
            ( Model model
            , Just
                (AddNoteEvent
                    noteEvent
                    (MidiEvent.fromNoteEvent noteEvent |> .pitch)
                )
            )

        UserClickedRightMouseButton coordinate ->
            let
                noteEvent : Music.NoteEvent
                noteEvent =
                    fromMusicToNoteEvent coordinate
            in
            ( Model model
            , Just (RemoveNoteEvent noteEvent)
            )


view :
    { project : Project.Project
    , model : Model
    , toMsg : Msg -> msg
    }
    -> Html msg
view options =
    let
        (Model model) =
            options.model
    in
    Html.div
        [ Html.Attributes.class "piano-roll__wrapper" ]
        [ viewRoll options
        , viewNotes options.project
        , case model.mousePosition of
            Just coordinate ->
                viewNote "mediumseagreen"
                    (fromMusicToNoteEvent coordinate)

            Nothing ->
                Html.text ""
        ]


viewRoll :
    { project : Project.Project
    , model : Model
    , toMsg : Msg -> msg
    }
    -> Html msg
viewRoll options =
    Html.div
        ([ Html.Attributes.class "piano-roll"
         , Html.Attributes.style "background-image"
            (backgroundImageAttr
                { width = cellSizeX, height = cellSizeY }
            )
         ]
            ++ mouseEvents
        )
        []
        |> Html.map options.toMsg


viewNotes : Project.Project -> Html msg
viewNotes project =
    Project.noteEvents project
        |> List.map (viewNote "deeppink")
        |> Html.div []


viewNote : String -> Music.NoteEvent -> Html msg
viewNote color noteEvent =
    let
        { x, y } =
            MusicCoordinate noteEvent.at (Music.Note.pitch noteEvent.value)
                |> fromMusicToPixels

        width : Int
        width =
            cellSizeX

        height : Int
        height =
            cellSizeY
    in
    Html.div
        [ Html.Attributes.class "note-preview"
        , Html.Attributes.style "background" color
        , Html.Attributes.style "width" (String.fromInt width ++ "px")
        , Html.Attributes.style "height" (String.fromInt height ++ "px")
        , Html.Attributes.style "transform"
            ("translate($x, $y)"
                |> String.replace "$x" (String.fromInt x ++ "px")
                |> String.replace "$y" (String.fromInt y ++ "px")
            )
        ]
        []


mouseEvents : List (Html.Attribute Msg)
mouseEvents =
    let
        onMouseMove : Html.Attribute Msg
        onMouseMove =
            Html.Events.on "mousemove"
                (offsetDecoder
                    |> Json.Decode.map fromPixelsToMusic
                    |> Json.Decode.map UserMovedMouseOverGrid
                )

        onMouseUp : Html.Attribute Msg
        onMouseUp =
            Html.Events.on "mouseup"
                (Json.Decode.andThen
                    (\mouseButton ->
                        if mouseButton == 0 then
                            offsetDecoder
                                |> Json.Decode.map fromPixelsToMusic
                                |> Json.Decode.map UserClickedLeftMouseButton

                        else if mouseButton == 2 then
                            offsetDecoder
                                |> Json.Decode.map fromPixelsToMusic
                                |> Json.Decode.map UserClickedRightMouseButton

                        else
                            Json.Decode.fail "bad witch club 🧙\u{200D}♀️"
                    )
                    (Json.Decode.field "button" Json.Decode.int)
                )

        offsetDecoder : Json.Decode.Decoder PixelCoordinate
        offsetDecoder =
            Json.Decode.map2
                PixelCoordinate
                (Json.Decode.field "offsetX" Json.Decode.int)
                (Json.Decode.field "offsetY" Json.Decode.int)
    in
    [ onMouseMove
    , onMouseUp
    , Html.Events.onMouseLeave UserMovedMouseAway
    ]


backgroundImageAttr : { height : Int, width : Int } -> String
backgroundImageAttr options =
    gridBackground
        |> String.replace "$keys" (PianoRoll.Key.view options.height)
        |> String.replace "$verticalLines"
            (List.range 0 4
                |> List.map (viewVerticalLine options)
                |> String.join ""
            )
        |> String.replace "$totalHeight" (String.fromInt (12 * options.height))
        |> String.replace "$midSplit" (String.fromInt (7 * options.height))
        |> String.replace "$verticalLineColor" "#fff5"
        |> String.replace "$horizontalLineColor" "#fff2"
        |> String.replace "$width" (String.fromInt (4 * options.width))
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



-- Coordinate conversions


type alias PixelCoordinate =
    { x : Int, y : Int }


type alias MusicCoordinate =
    { at : Music.Duration.Duration
    , pitch : Music.Pitch.Pitch
    }


pixelsXToStart : Int -> Music.Duration.Duration
pixelsXToStart x =
    Music.Duration.multiplyByInt (x // cellSizeX) Music.Duration.quarter


startToPixelsX : Music.Duration.Duration -> Int
startToPixelsX duration =
    Basics.round (Music.Duration.toFloat duration * 4) * cellSizeX


pixelsYToPitch : Int -> Music.Pitch.Pitch
pixelsYToPitch y =
    (131 - (y // cellSizeY))
        |> Music.Pitch.fromMIDINoteNumber


pitchToPixelsY : Music.Pitch.Pitch -> Int
pitchToPixelsY pitch =
    (131 - Music.Pitch.toMIDINoteNumber pitch)
        * cellSizeY


fromPixelsToMusic : PixelCoordinate -> MusicCoordinate
fromPixelsToMusic { x, y } =
    MusicCoordinate (pixelsXToStart x) (pixelsYToPitch y)


fromMusicToPixels : MusicCoordinate -> PixelCoordinate
fromMusicToPixels { at, pitch } =
    { x = startToPixelsX at
    , y = pitchToPixelsY pitch
    }


fromMusicToNoteEvent : MusicCoordinate -> Music.NoteEvent
fromMusicToNoteEvent { at, pitch } =
    { at = at
    , value = Music.Note.quarter pitch
    }
