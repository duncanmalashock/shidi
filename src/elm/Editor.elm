module Editor exposing
    ( Model, init
    , Msg, update, OutMsg(..)
    , view
    )

{-|

@docs Model, init

@docs Msg, update, OutMsg

@docs view

-}

import Editor.Coordinate
import Editor.Key
import Editor.Measure
import Editor.Piano
import Editor.Zoom
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Json.Decode
import Music
import Music.Duration
import Music.Event as Event
import Music.Note
import Music.Pitch
import Project


type Model
    = Model
        { mousePosition : Maybe Editor.Coordinate.MusicCoordinate
        , zoom : Editor.Zoom.Zoom
        }


init : Model
init =
    Model
        { mousePosition = Nothing
        , zoom = Editor.Zoom.new
        }


type Msg
    = UserMovedMouseOverGrid Editor.Coordinate.MusicCoordinate
    | UserMovedMouseAway
    | UserClickedLeftMouseButton Editor.Coordinate.MusicCoordinate
    | UserClickedRightMouseButton Editor.Coordinate.MusicCoordinate
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
                    Editor.Coordinate.fromMusicToPitchEvent coordinate
            in
            ( Model model
            , Just
                (ShouldAddNote pitchEvent)
            )

        UserClickedRightMouseButton coordinate ->
            let
                pitchEvent : PitchEvent
                pitchEvent =
                    Editor.Coordinate.fromMusicToPitchEvent coordinate
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
            { zoom = model.zoom
            , toMsg = options.toMsg
            , measures = Project.measures options.project
            }
        , viewPianoRoll
            { zoom = model.zoom
            , toMsg = options.toMsg
            , measures = Project.measures options.project
            , noteEvents = Project.noteEvents options.project
            , mousePosition = model.mousePosition
            , newNoteValue = options.newNoteValue
            }
        ]


viewMetadata :
    { zoom : Editor.Zoom.Zoom
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
                , Html.Attributes.style "width" "37px"
                ]
                []

        viewMeasures : Html msg
        viewMeasures =
            Html.div
                [ Html.Attributes.class "metadata__measures"
                ]
                (List.indexedMap
                    (viewMetadataMeasure
                        { zoom = options.zoom
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
    { zoom : Editor.Zoom.Zoom
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
                (Editor.Zoom.cellSizeX options.zoom * 8)
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
    { zoom : Editor.Zoom.Zoom
    , toMsg : Msg -> msg
    , measures : List Music.Measure
    , noteEvents : List (Event.Event Music.Note.Note)
    , newNoteValue : Music.Duration.Duration
    , mousePosition : Maybe Editor.Coordinate.MusicCoordinate
    }
    -> Html msg
viewPianoRoll options =
    let
        viewPiano : Html msg
        viewPiano =
            Html.div
                [ Html.Attributes.class "piano-roll__piano"
                ]
                [ Editor.Piano.view
                    (Editor.Zoom.cellSizeY options.zoom)
                    (UserClickedPianoKey >> options.toMsg)
                ]

        viewNotePreview : Html msg
        viewNotePreview =
            case options.mousePosition of
                Just coordinate ->
                    viewNote
                        { zoom = options.zoom
                        , color = "#ffffff22"
                        }
                        { at = coordinate.at
                        , value = Music.Note.note coordinate.pitch options.newNoteValue
                        }

                Nothing ->
                    Html.text ""

        viewContent : Html msg
        viewContent =
            Html.div
                [ Html.Attributes.class "piano-roll__content"
                ]
                (List.map
                    (Editor.Measure.viewMeasure
                        { zoom = options.zoom
                        , onMovedMouseOverGrid = UserMovedMouseOverGrid >> options.toMsg
                        , onClickedLeftMouseButton = UserClickedLeftMouseButton >> options.toMsg
                        , onClickedRightMouseButton = UserClickedRightMouseButton >> options.toMsg
                        , onMovedMouseAway = UserMovedMouseAway |> options.toMsg
                        }
                    )
                    options.measures
                    ++ [ viewNotes
                            { zoom = options.zoom
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
        , viewContent
        ]


viewNotes :
    { zoom : Editor.Zoom.Zoom
    , noteEvents : List (Event.Event Music.Note.Note)
    }
    -> Html msg
viewNotes options =
    Html.div []
        (List.map
            (viewNote
                { zoom = options.zoom
                , color = "deeppink"
                }
            )
            options.noteEvents
        )


viewNote :
    { zoom : Editor.Zoom.Zoom
    , color : String
    }
    -> Event.Event Music.Note.Note
    -> Html msg
viewNote options noteEvent =
    let
        { x, y } =
            Editor.Coordinate.MusicCoordinate noteEvent.at (Music.Note.pitch noteEvent.value)
                |> Editor.Coordinate.fromMusicToPixels options.zoom

        noteDuration : Music.Duration.Duration
        noteDuration =
            Music.Note.duration noteEvent.value

        width : Int
        width =
            Music.Duration.multiplyByInt
                (Editor.Zoom.cellSizeX options.zoom * 8)
                noteDuration
                |> Music.Duration.toFloat
                |> Basics.round

        height : Int
        height =
            Editor.Zoom.cellSizeY options.zoom
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
