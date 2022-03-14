module Main exposing (main)

import Browser
import Browser.Dom
import Coordinate
import File
import File.Load
import File.Save
import File.Select
import Html exposing (Html)
import Html.Attributes as Attr
import Html.Events
import Json.Decode
import MidiEvent
import Music
import Piano
import PianoRoll
import Ports
import Song
import Task


main : Program () Model Msg
main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type alias Model =
    { mousePosition : Maybe Coordinate.Pixels
    , song : Song.Song
    , fileName : String
    , showSaveModal : Bool
    }


init : () -> ( Model, Cmd Msg )
init flags =
    ( initialModel
    , Cmd.none
    )


initialModel : Model
initialModel =
    { mousePosition = Nothing
    , song = Song.empty
    , fileName = ""
    , showSaveModal = False
    }


type Msg
    = -- Piano keys
      UserClickedPianoKey Int
      -- Piano roll
    | UserMovedMouseOverPianoRoll Coordinate.Pixels
    | UserMovedMouseOutOfPianoRoll
    | UserClickedPianoRoll Coordinate.Pixels
    | UserRightClickedPianoRoll Coordinate.Pixels
      -- Playback
    | UserClickedPlayButton
      -- Saving to file
    | UserClickedSaveButton
    | UserClickedModalSaveButton
    | AppFocusedOnFileNameField (Result Browser.Dom.Error ())
    | UserTypedIntoNameField String
    | UserDismissedSaveModal
      -- Loading from file
    | UserClickedLoadButton
    | UserSelectedFile File.File
    | AppLoadedFile (Result Json.Decode.Error Song.Song)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UserMovedMouseOverPianoRoll position ->
            ( { model | mousePosition = Just position }
            , Cmd.none
            )

        UserMovedMouseOutOfPianoRoll ->
            ( { model | mousePosition = Nothing }
            , Cmd.none
            )

        UserClickedPianoKey note ->
            ( model, Ports.playNote note )

        UserClickedPianoRoll coordinate ->
            let
                newNoteEvent : Music.NoteEvent
                newNoteEvent =
                    coordinate
                        |> Coordinate.fromPixelsToMusic
                        |> Coordinate.fromMusicToNoteEvent

                midiPitch : Int
                midiPitch =
                    MidiEvent.fromNoteEvent newNoteEvent
                        |> .pitch
            in
            ( { model
                | song =
                    Song.addNote newNoteEvent model.song
              }
            , Ports.playNote midiPitch
            )

        UserRightClickedPianoRoll coordinate ->
            let
                noteEvent : Music.NoteEvent
                noteEvent =
                    coordinate
                        |> Coordinate.fromPixelsToMusic
                        |> Coordinate.fromMusicToNoteEvent
            in
            ( { model
                | song =
                    Song.removeNote
                        noteEvent
                        model.song
              }
            , Cmd.none
            )

        UserClickedPlayButton ->
            ( model, Ports.playSong model.song )

        UserClickedSaveButton ->
            ( { model
                | showSaveModal = True
              }
            , Task.attempt AppFocusedOnFileNameField (Browser.Dom.focus "filename-input")
            )

        AppFocusedOnFileNameField _ ->
            ( model, Cmd.none )

        UserClickedLoadButton ->
            ( model, File.Select.file [ "text/shidi" ] UserSelectedFile )

        UserSelectedFile file ->
            ( { model
                | fileName =
                    File.name file
                        |> String.dropRight (String.length ".shidi")
              }
            , File.Load.load AppLoadedFile file
            )

        AppLoadedFile result ->
            case result of
                Ok song ->
                    ( { model | song = song }, Cmd.none )

                Err error ->
                    ( model, Cmd.none )

        UserClickedModalSaveButton ->
            ( { model | showSaveModal = False }
            , File.Save.save model.fileName model.song
            )

        UserTypedIntoNameField newFileName ->
            ( { model | fileName = newFileName }, Cmd.none )

        UserDismissedSaveModal ->
            ( { model | showSaveModal = False }
            , Cmd.none
            )


view : Model -> { title : String, body : List (Html Msg) }
view model =
    { title = "shidi"
    , body =
        [ Html.div [ Attr.class "row" ]
            [ Piano.view UserClickedPianoKey
            , Html.div
                [ Attr.class "piano-roll__wrapper" ]
                [ PianoRoll.view
                    { onMouseMove = UserMovedMouseOverPianoRoll
                    , onMouseLeave = UserMovedMouseOutOfPianoRoll
                    , onLeftClick = UserClickedPianoRoll
                    , onRightClick = UserRightClickedPianoRoll
                    }
                , viewNotes model.song
                , case model.mousePosition of
                    Just coordinate ->
                        viewNote "mediumseagreen"
                            (Coordinate.fromPixelsToMusic coordinate
                                |> Coordinate.fromMusicToNoteEvent
                            )

                    Nothing ->
                        Html.text ""
                ]
            , viewPlayButton
            , viewSaveButton
            , viewLoadButton
            ]
        , viewFileSaveDialog model
        ]
    }


viewFileSaveDialog : Model -> Html Msg
viewFileSaveDialog model =
    if model.showSaveModal then
        Html.div []
            [ Html.div
                [ Attr.class "modal-dismiss"
                , Html.Events.onClick UserDismissedSaveModal
                ]
                []
            , Html.form
                [ Attr.class "modal"
                , Html.Events.onSubmit UserClickedModalSaveButton
                ]
                [ Html.input
                    [ Attr.type_ "text"
                    , Html.Events.onInput UserTypedIntoNameField
                    , Attr.value model.fileName
                    , Attr.id "filename-input"
                    ]
                    []
                , Html.button
                    []
                    [ Html.text "Save" ]
                ]
            ]

    else
        Html.text ""


viewPlayButton : Html Msg
viewPlayButton =
    Html.button
        [ Attr.class "play-button"
        , Html.Events.onClick UserClickedPlayButton
        ]
        [ Html.text "Play" ]


viewSaveButton : Html Msg
viewSaveButton =
    Html.button
        [ Attr.class "save-button"
        , Html.Events.onClick UserClickedSaveButton
        ]
        [ Html.text "Save" ]


viewLoadButton : Html Msg
viewLoadButton =
    Html.button
        [ Attr.class "load-button"
        , Html.Events.onClick UserClickedLoadButton
        ]
        [ Html.text "Load" ]


viewNotes : Song.Song -> Html Msg
viewNotes song =
    Song.notes song
        |> List.map (viewNote "deeppink")
        |> Html.div []


viewNote : String -> Music.NoteEvent -> Html Msg
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


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
