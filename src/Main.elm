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
import Project
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
    { mousePosition : Maybe Coordinate.Music
    , project : Project.Project
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
    , project = Project.empty
    , fileName = ""
    , showSaveModal = False
    }


type Msg
    = -- Piano keys
      UserClickedPianoKey Int
      -- Piano roll
    | UserMovedMouseOverPianoRoll Coordinate.Music
    | UserMovedMouseOutOfPianoRoll
    | UserClickedPianoRoll Coordinate.Music
    | UserRightClickedPianoRoll Coordinate.Music
      -- Playback
    | UserClickedPlayButton
      -- Saving to file
    | UserClickedSaveButton
    | UserClickedModalSaveButton
    | BrowserFocusedOnFileNameField (Result Browser.Dom.Error ())
    | UserTypedIntoNameField String
    | UserDismissedSaveModal
      -- Loading from file
    | UserClickedLoadButton
    | UserSelectedFile File.File
    | BrowserLoadedFile (Result Json.Decode.Error Project.Project)


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
                        |> Coordinate.fromMusicToNoteEvent

                midiPitch : Int
                midiPitch =
                    MidiEvent.fromNoteEvent newNoteEvent
                        |> .pitch
            in
            ( { model
                | project =
                    Project.addNote newNoteEvent model.project
              }
            , Ports.playNote midiPitch
            )

        UserRightClickedPianoRoll coordinate ->
            let
                noteEvent : Music.NoteEvent
                noteEvent =
                    coordinate
                        |> Coordinate.fromMusicToNoteEvent
            in
            ( { model
                | project =
                    Project.removeNote
                        noteEvent
                        model.project
              }
            , Cmd.none
            )

        UserClickedPlayButton ->
            ( model, Ports.playSong model.project )

        UserClickedSaveButton ->
            ( { model
                | showSaveModal = True
              }
            , Task.attempt BrowserFocusedOnFileNameField (Browser.Dom.focus "filename-input")
            )

        BrowserFocusedOnFileNameField _ ->
            ( model, Cmd.none )

        UserClickedLoadButton ->
            ( model, File.Select.file [ "text/shidi" ] UserSelectedFile )

        UserSelectedFile file ->
            ( { model
                | fileName =
                    File.name file
                        |> String.dropRight (String.length ".shidi")
              }
            , File.Load.load BrowserLoadedFile file
            )

        BrowserLoadedFile result ->
            case result of
                Ok project ->
                    ( { model | project = project }, Cmd.none )

                Err error ->
                    ( model, Cmd.none )

        UserClickedModalSaveButton ->
            ( { model | showSaveModal = False }
            , File.Save.save model.fileName model.project
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
            , PianoRoll.view
                { onMouseMove = UserMovedMouseOverPianoRoll
                , onMouseLeave = UserMovedMouseOutOfPianoRoll
                , onLeftClick = UserClickedPianoRoll
                , onRightClick = UserRightClickedPianoRoll
                , project = model.project
                , mousePosition = model.mousePosition
                }
            , viewPlayButton
            , viewSaveButton
            , viewLoadButton
            ]
        , viewFileSaveModal model
        ]
    }


viewFileSaveModal : Model -> Html Msg
viewFileSaveModal model =
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


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
