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
import Editor.Measure
import Editor.Piano
import Html exposing (Html)
import Html.Attributes
import Music
import Music.Duration
import Music.Event as Event
import Music.Note
import Music.Pitch
import Project
import Zoom


type Model
    = Model
        { mousePosition : Maybe Editor.Coordinate.Music
        }


init : Model
init =
    Model
        { mousePosition = Nothing
        }


type Msg
    = UserMovedMouseOverGrid Editor.Coordinate.Music
    | UserMovedMouseAway
    | UserClickedLeftMouseButton Editor.Coordinate.Music
    | UserClickedRightMouseButton Editor.Coordinate.Music
    | UserClickedPianoKey Int


type OutMsg
    = ShouldAddNote PitchEvent
    | ShouldRemoveNote PitchEvent
    | ShouldPlayNote Int


type alias PitchEvent =
    { at : Music.Duration.Duration
    , value : Music.Pitch.Pitch
    }


fromMusicToPitchEvent : Editor.Coordinate.Music -> PitchEvent
fromMusicToPitchEvent { at, pitch } =
    { at = at
    , value = pitch
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
    , zoom : Zoom.Zoom
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
            { zoom = options.zoom
            , toMsg = options.toMsg
            , measures = Project.measures options.project
            }
        , viewPianoRoll
            { zoom = options.zoom
            , toMsg = options.toMsg
            , measures = Project.measures options.project
            , noteEvents = Project.noteEvents options.project
            , mousePosition = model.mousePosition
            , newNoteValue = options.newNoteValue
            }
        ]


viewMetadata :
    { zoom : Zoom.Zoom
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
                    (Editor.Measure.viewMetadata
                        { zoom = options.zoom
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


viewPianoRoll :
    { zoom : Zoom.Zoom
    , toMsg : Msg -> msg
    , measures : List Music.Measure
    , noteEvents : List (Event.Event Music.Note.Note)
    , newNoteValue : Music.Duration.Duration
    , mousePosition : Maybe Editor.Coordinate.Music
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
                    (Zoom.cellSizeY options.zoom)
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
                    (Editor.Measure.view
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
    { zoom : Zoom.Zoom
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
    { zoom : Zoom.Zoom
    , color : String
    }
    -> Event.Event Music.Note.Note
    -> Html msg
viewNote options noteEvent =
    let
        { x, y } =
            Editor.Coordinate.Music noteEvent.at (Music.Note.pitch noteEvent.value)
                |> Editor.Coordinate.fromMusicToPixels options.zoom

        noteDuration : Music.Duration.Duration
        noteDuration =
            Music.Note.duration noteEvent.value

        width : Int
        width =
            Music.Duration.multiplyByInt
                (Zoom.cellSizeX options.zoom * 8)
                noteDuration
                |> Music.Duration.toFloat
                |> Basics.round

        height : Int
        height =
            Zoom.cellSizeY options.zoom
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
