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
import Music
import Music.Duration
import Music.Event as Event
import Music.Note
import Music.Pitch
import PianoRoll.Key
import PianoRoll.Piano
import Project


type ScaleX
    = ScaleXSmall
    | ScaleXMedium
    | ScaleXLarge
    | ScaleXGiant


cellSizeX : ScaleX -> Int
cellSizeX scaleX =
    case scaleX of
        ScaleXSmall ->
            14

        ScaleXMedium ->
            21

        ScaleXLarge ->
            36

        ScaleXGiant ->
            50


type ScaleY
    = ScaleYSmall
    | ScaleYMedium
    | ScaleYLarge


cellSizeY : ScaleY -> Int
cellSizeY scaleY =
    case scaleY of
        ScaleYSmall ->
            14

        ScaleYMedium ->
            21

        ScaleYLarge ->
            28


type Model
    = Model
        { mousePosition : Maybe MusicCoordinate
        , scaleX : ScaleX
        , scaleY : ScaleY
        }


init : Model
init =
    Model
        { mousePosition = Nothing
        , scaleX = ScaleXMedium
        , scaleY = ScaleYMedium
        }


type Msg
    = UserMovedMouseOverGrid MusicCoordinate
    | UserMovedMouseAway
    | UserClickedLeftMouseButton MusicCoordinate
    | UserClickedRightMouseButton MusicCoordinate
    | UserClickedPianoKey Int


type OutMsg
    = ShouldAddNote PitchEvent
    | ShouldRemoveNote PitchEvent
    | ShouldPlayNote Int


type alias PitchEvent =
    { at : Music.Duration.Duration
    , value : Music.Pitch.Pitch
    }


update :
    Msg
    -> Model
    -> ( Model, Maybe OutMsg )
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
                pitchEvent : PitchEvent
                pitchEvent =
                    fromMusicToPitchEvent coordinate
            in
            ( Model model
            , Just
                (ShouldAddNote pitchEvent)
            )

        UserClickedRightMouseButton coordinate ->
            let
                pitchEvent : PitchEvent
                pitchEvent =
                    fromMusicToPitchEvent coordinate
            in
            ( Model model
            , Just (ShouldRemoveNote pitchEvent)
            )

        UserClickedPianoKey noteNumber ->
            ( Model model
            , Just (ShouldPlayNote noteNumber)
            )


view :
    { project : Project.Project
    , model : Model
    , toMsg : Msg -> msg
    , newNoteValue : Music.Duration.Duration
    }
    -> Html msg
view options =
    let
        (Model model) =
            options.model
    in
    Html.div
        [ Html.Attributes.class "editor"
        ]
        [ viewMetadata
            { scaleX = model.scaleX
            , scaleY = model.scaleY
            , toMsg = options.toMsg
            , measures = Project.measures options.project
            }
        , viewPianoRoll
            { scaleX = model.scaleX
            , scaleY = model.scaleY
            , toMsg = options.toMsg
            , measures = Project.measures options.project
            , noteEvents = Project.noteEvents options.project
            , mousePosition = model.mousePosition
            , newNoteValue = options.newNoteValue
            }
        ]


viewMetadata :
    { scaleX : ScaleX
    , scaleY : ScaleY
    , toMsg : Msg -> msg
    , measures : List Music.Measure
    }
    -> Html msg
viewMetadata options =
    let
        viewShim : Html msg
        viewShim =
            Html.div
                [ Html.Attributes.class "metadata__shim"
                ]
                []

        viewMeasures : Html msg
        viewMeasures =
            Html.div
                [ Html.Attributes.class "metadata__measures"
                ]
                (List.indexedMap
                    (viewMetadataMeasure
                        { scaleX = options.scaleX
                        , scaleY = options.scaleY
                        , toMsg = options.toMsg
                        }
                    )
                    options.measures
                )
    in
    Html.div
        [ Html.Attributes.class "metadata"
        ]
        [ viewShim
        , viewMeasures
        ]


viewMetadataMeasure :
    { scaleX : ScaleX
    , scaleY : ScaleY
    , toMsg : Msg -> msg
    }
    -> Int
    -> Music.Measure
    -> Html msg
viewMetadataMeasure options index measure =
    let
        width : Int
        width =
            Music.Duration.multiplyByInt
                (cellSizeX options.scaleX * 8)
                Music.Duration.eighth
                |> Music.Duration.toFloat
                |> Basics.round
                |> (*) 8
    in
    Html.div
        [ Html.Attributes.class "metadata__measure"
        , Html.Attributes.style "width" (String.fromInt width ++ "px")
        ]
        [ Html.text (String.fromInt (index + 1))
        ]
        |> Html.map options.toMsg


viewPianoRoll :
    { scaleX : ScaleX
    , scaleY : ScaleY
    , toMsg : Msg -> msg
    , measures : List Music.Measure
    , noteEvents : List (Event.Event Music.Note.Note)
    , newNoteValue : Music.Duration.Duration
    , mousePosition : Maybe MusicCoordinate
    }
    -> Html msg
viewPianoRoll options =
    let
        viewPiano : Html msg
        viewPiano =
            Html.div
                [ Html.Attributes.class "piano-roll__piano"
                ]
                [ PianoRoll.Piano.view
                    (cellSizeY options.scaleY)
                    (UserClickedPianoKey >> options.toMsg)
                ]

        viewNotePreview =
            case options.mousePosition of
                Just coordinate ->
                    viewNote
                        { scaleX = options.scaleX
                        , scaleY = options.scaleY
                        , color = "#ffffff22"
                        }
                        { at = coordinate.at
                        , value = Music.Note.note coordinate.pitch options.newNoteValue
                        }

                Nothing ->
                    Html.text ""

        viewMeasures : Html msg
        viewMeasures =
            Html.div
                [ Html.Attributes.class "piano-roll__measures"
                ]
                (List.map
                    (viewMeasure
                        { scaleX = options.scaleX
                        , scaleY = options.scaleY
                        , toMsg = options.toMsg
                        }
                    )
                    options.measures
                    ++ [ viewNotes
                            { scaleX = options.scaleX
                            , scaleY = options.scaleY
                            , noteEvents = options.noteEvents
                            }
                       , viewNotePreview
                       ]
                )
    in
    Html.div
        [ Html.Attributes.class "piano-roll"
        ]
        [ viewPiano
        , viewMeasures
        ]


viewMeasure :
    { scaleX : ScaleX
    , scaleY : ScaleY
    , toMsg : Msg -> msg
    }
    -> Music.Measure
    -> Html msg
viewMeasure options measure =
    Html.div
        ([ Html.Attributes.class "piano-roll__measure"
         , Html.Attributes.style "width" "calc(8 * 21px)"
         , Html.Attributes.style "background-image"
            (backgroundImageAttr
                { width = cellSizeX options.scaleX
                , height = cellSizeY options.scaleY
                }
            )
         ]
            ++ mouseEvents measure options.scaleX options.scaleY
        )
        []
        |> Html.map options.toMsg


viewNotes :
    { scaleX : ScaleX
    , scaleY : ScaleY
    , noteEvents : List (Event.Event Music.Note.Note)
    }
    -> Html msg
viewNotes options =
    Html.div []
        (List.map
            (viewNote
                { scaleX = options.scaleX
                , scaleY = options.scaleY
                , color = "deeppink"
                }
            )
            options.noteEvents
        )


viewNote :
    { scaleX : ScaleX
    , scaleY : ScaleY
    , color : String
    }
    -> Event.Event Music.Note.Note
    -> Html msg
viewNote options noteEvent =
    let
        { x, y } =
            MusicCoordinate noteEvent.at (Music.Note.pitch noteEvent.value)
                |> fromMusicToPixels options.scaleX options.scaleY

        noteDuration : Music.Duration.Duration
        noteDuration =
            Music.Note.duration noteEvent.value

        width : Int
        width =
            Music.Duration.multiplyByInt
                (cellSizeX options.scaleX * 8)
                noteDuration
                |> Music.Duration.toFloat
                |> Basics.round

        height : Int
        height =
            cellSizeY options.scaleY
    in
    Html.div
        [ Html.Attributes.class "note-preview"
        , Html.Attributes.style "background" options.color
        , Html.Attributes.style "border" "1px solid #ffffff66"
        , Html.Attributes.style "box-sizing" "border-box"
        , Html.Attributes.style "width" (String.fromInt width ++ "px")
        , Html.Attributes.style "height" (String.fromInt height ++ "px")
        , Html.Attributes.style "transform"
            ("translate($x, $y)"
                |> String.replace "$x" (String.fromInt x ++ "px")
                |> String.replace "$y" (String.fromInt y ++ "px")
            )
        ]
        []


mouseEvents : Music.Measure -> ScaleX -> ScaleY -> List (Html.Attribute Msg)
mouseEvents measure scaleX scaleY =
    let
        onMouseMove : Html.Attribute Msg
        onMouseMove =
            Html.Events.on "mousemove"
                (offsetDecoder
                    |> Json.Decode.map (fromPixelsToMusic measure scaleX scaleY)
                    |> Json.Decode.map UserMovedMouseOverGrid
                )

        onMouseUp : Html.Attribute Msg
        onMouseUp =
            Html.Events.on "mouseup"
                (Json.Decode.andThen
                    (\mouseButton ->
                        if mouseButton == 0 then
                            offsetDecoder
                                |> Json.Decode.map (fromPixelsToMusic measure scaleX scaleY)
                                |> Json.Decode.map UserClickedLeftMouseButton

                        else if mouseButton == 2 then
                            offsetDecoder
                                |> Json.Decode.map (fromPixelsToMusic measure scaleX scaleY)
                                |> Json.Decode.map UserClickedRightMouseButton

                        else
                            Json.Decode.fail "bad witch club"
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



-- Coordinate conversions


type alias PixelCoordinate =
    { x : Int, y : Int }


type alias MusicCoordinate =
    { at : Music.Duration.Duration
    , pitch : Music.Pitch.Pitch
    }


pixelsXToStart : ScaleX -> Int -> Music.Duration.Duration
pixelsXToStart scaleX x =
    Music.Duration.multiplyByInt (x // cellSizeX scaleX) Music.Duration.eighth


startToPixelsX : ScaleX -> Music.Duration.Duration -> Int
startToPixelsX scaleX duration =
    Basics.round (Music.Duration.toFloat duration * 8) * cellSizeX scaleX


pixelsYToPitch : ScaleY -> Int -> Music.Pitch.Pitch
pixelsYToPitch scaleY y =
    (131 - (y // cellSizeY scaleY))
        |> Music.Pitch.fromMIDINoteNumber


pitchToPixelsY : ScaleY -> Music.Pitch.Pitch -> Int
pitchToPixelsY scaleY pitch =
    (131 - Music.Pitch.toMIDINoteNumber pitch)
        * cellSizeY scaleY


fromPixelsToMusic : Music.Measure -> ScaleX -> ScaleY -> PixelCoordinate -> MusicCoordinate
fromPixelsToMusic measure scaleX scaleY { x, y } =
    let
        xDuration : Music.Duration.Duration
        xDuration =
            Music.Duration.add measure.start (pixelsXToStart scaleX x)
    in
    MusicCoordinate xDuration (pixelsYToPitch scaleY y)


fromMusicToPixels : ScaleX -> ScaleY -> MusicCoordinate -> PixelCoordinate
fromMusicToPixels scaleX scaleY { at, pitch } =
    { x = startToPixelsX scaleX at
    , y = pitchToPixelsY scaleY pitch
    }


fromMusicToPitchEvent : MusicCoordinate -> PitchEvent
fromMusicToPitchEvent { at, pitch } =
    { at = at
    , value = pitch
    }
