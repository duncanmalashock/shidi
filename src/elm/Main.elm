module Main exposing (main)

import Browser
import Browser.Dom
import Editor
import File
import File.Load
import File.Save
import File.Select
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Json.Decode
import Music.Duration
import Music.Note
import Music.Pitch
import Music.Tempo as Tempo
import Ports
import Project
import Task
import Ui.Button


main : Program () Model Msg
main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type alias Model =
    { project : Project.Project
    , fileName : String
    , showSaveModal : Bool
    , editor : Editor.Model
    , newNoteDuration : Music.Duration.Duration
    , errorMessage : Maybe String
    }


init : () -> ( Model, Cmd Msg )
init flags =
    ( initialModel
    , Cmd.none
    )


initialModel : Model
initialModel =
    { project = Project.empty
    , fileName = ""
    , showSaveModal = False
    , editor = Editor.init
    , newNoteDuration = Music.Duration.whole
    , errorMessage = Nothing
    }


type
    Msg
    -- Editor
    = ClientReceivedEditorMsg Editor.Msg
      -- Playback
    | UserClickedPlayButton
      -- Saving to file
    | UserClickedSaveButton
    | UserClickedModalSaveButton
    | UserSubmittedSaveForm
    | ClientFocusedOnFileNameField (Result Browser.Dom.Error ())
    | UserTypedIntoNameField String
    | UserDismissedSaveModal
      -- Loading from file
    | UserClickedLoadButton
    | UserSelectedFile File.File
    | ClientLoadedFile (Result Json.Decode.Error Project.Project)
      -- Note add tool
    | UserClickedNoteValueButton Music.Duration.Duration
      -- Setting tempo
    | UserTypedIntoTempoField String
      -- Toast
    | UserClickedToastDismissButton


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ClientReceivedEditorMsg editorMsg ->
            let
                ( editor, maybeOutMsg ) =
                    Editor.update
                        editorMsg
                        model.editor

                { cmd, updatedProject } =
                    case maybeOutMsg of
                        Just (Editor.ShouldAddNote pitchEvent) ->
                            { cmd =
                                Music.Pitch.toMIDINoteNumber pitchEvent.value
                                    |> Ports.playNote
                            , updatedProject =
                                model.project
                                    |> Project.addNote
                                        { at = pitchEvent.at
                                        , value = Music.Note.note pitchEvent.value model.newNoteDuration
                                        }
                            }

                        Just (Editor.ShouldRemoveNote pitchEvent) ->
                            { cmd = Cmd.none
                            , updatedProject =
                                Project.removeNote
                                    { at = pitchEvent.at
                                    , value = Music.Note.note pitchEvent.value model.newNoteDuration
                                    }
                                    model.project
                            }

                        Just (Editor.ShouldPlayNote noteNumber) ->
                            { cmd = Ports.playNote noteNumber
                            , updatedProject = model.project
                            }

                        Nothing ->
                            { cmd = Cmd.none
                            , updatedProject = model.project
                            }
            in
            ( { model
                | editor = editor
                , project = updatedProject
              }
            , cmd
            )

        UserClickedPlayButton ->
            ( model, Ports.playSong model.project )

        UserClickedSaveButton ->
            ( { model
                | showSaveModal = True
              }
            , Task.attempt ClientFocusedOnFileNameField (Browser.Dom.focus "filename-input")
            )

        ClientFocusedOnFileNameField _ ->
            ( model, Cmd.none )

        UserClickedLoadButton ->
            ( model, File.Select.file [ "text/shidi" ] UserSelectedFile )

        UserSelectedFile file ->
            ( { model
                | fileName =
                    File.name file
                        |> String.dropRight (String.length ".shidi")
              }
            , File.Load.load ClientLoadedFile file
            )

        ClientLoadedFile result ->
            case result of
                Ok project ->
                    ( { model | project = project }, Cmd.none )

                Err _ ->
                    ( { model | errorMessage = Just "Invalid file format" }, Cmd.none )

        UserClickedModalSaveButton ->
            ( model
            , Cmd.none
            )

        UserSubmittedSaveForm ->
            ( { model | showSaveModal = False }
            , File.Save.save model.fileName model.project
            )

        UserTypedIntoNameField newFileName ->
            ( { model | fileName = newFileName }, Cmd.none )

        UserDismissedSaveModal ->
            ( { model | showSaveModal = False }
            , Cmd.none
            )

        UserClickedNoteValueButton duration ->
            ( { model
                | newNoteDuration = duration
              }
            , Cmd.none
            )

        UserTypedIntoTempoField newTempoString ->
            let
                newTempo : Tempo.Tempo
                newTempo =
                    case String.toInt newTempoString of
                        Just tempoBPM ->
                            Tempo.quarterNotesPerMinute tempoBPM

                        Nothing ->
                            Project.tempo model.project
            in
            ( { model | project = Project.setInitialTempo newTempo model.project }
            , Cmd.none
            )

        UserClickedToastDismissButton ->
            ( { model | errorMessage = Nothing }
            , Cmd.none
            )


view : Model -> { title : String, body : List (Html Msg) }
view model =
    { title = "shidi"
    , body =
        [ viewFileSaveModal model
        , viewAppFrame model
        , viewToast model
        ]
    }


viewAppFrame : Model -> Html Msg
viewAppFrame model =
    let
        viewHeader : Html Msg
        viewHeader =
            Html.div
                [ Html.Attributes.class "app-frame__header" ]
                [ viewControls model
                ]

        viewContent : Html Msg
        viewContent =
            Html.div
                [ Html.Attributes.class "app-frame__content" ]
                [ Editor.view
                    { project = model.project
                    , model = model.editor
                    , toMsg = ClientReceivedEditorMsg
                    , newNoteValue = model.newNoteDuration
                    }
                ]
    in
    Html.div
        [ Html.Attributes.class "app-frame" ]
        [ viewHeader
        , viewContent
        ]


viewControls : Model -> Html Msg
viewControls model =
    Html.div
        [ Html.Attributes.class "controls" ]
        [ viewPlayButton
        , viewSaveButton
        , viewLoadButton
        , viewNoteValueButtons
        , viewTempoField (Project.tempo model.project)
        ]


viewToast : Model -> Html Msg
viewToast model =
    case model.errorMessage of
        Just message ->
            Html.div
                [ Html.Attributes.class "toast" ]
                [ Html.text message
                , Html.button
                    [ Html.Attributes.class "toast__dismiss"
                    , Html.Events.onClick UserClickedToastDismissButton
                    ]
                    [ Html.text "x" ]
                ]

        Nothing ->
            Html.text ""


viewFileSaveModal : Model -> Html Msg
viewFileSaveModal model =
    if model.showSaveModal then
        Html.div
            []
            [ Html.div
                [ Html.Attributes.class "modal-dismiss"
                , Html.Events.onClick UserDismissedSaveModal
                ]
                []
            , Html.form
                [ Html.Attributes.class "modal"
                , Html.Events.onSubmit UserSubmittedSaveForm
                ]
                [ Html.input
                    [ Html.Attributes.type_ "text"
                    , Html.Events.onInput UserTypedIntoNameField
                    , Html.Attributes.value model.fileName
                    , Html.Attributes.id "filename-input"
                    ]
                    []
                , Ui.Button.view
                    { label = "Save"
                    , onClick = UserClickedModalSaveButton
                    }
                ]
            ]

    else
        Html.text ""


viewPlayButton : Html Msg
viewPlayButton =
    Ui.Button.view
        { label = "Play"
        , onClick = UserClickedPlayButton
        }


viewSaveButton : Html Msg
viewSaveButton =
    Ui.Button.view
        { label = "Save"
        , onClick = UserClickedSaveButton
        }


viewLoadButton : Html Msg
viewLoadButton =
    Ui.Button.view
        { label = "Load"
        , onClick = UserClickedLoadButton
        }


viewNoteValueButtons : Html Msg
viewNoteValueButtons =
    Html.div
        []
        [ Ui.Button.view
            { label = "1/8"
            , onClick = UserClickedNoteValueButton Music.Duration.eighth
            }
        , Ui.Button.view
            { label = "1/4"
            , onClick = UserClickedNoteValueButton Music.Duration.quarter
            }
        , Ui.Button.view
            { label = "1/2"
            , onClick = UserClickedNoteValueButton Music.Duration.half
            }
        , Ui.Button.view
            { label = "1"
            , onClick = UserClickedNoteValueButton Music.Duration.whole
            }
        ]


viewTempoField : Tempo.Tempo -> Html Msg
viewTempoField tempo =
    let
        tempoAsString : String
        tempoAsString =
            tempo
                |> Tempo.toSerial
                |> .beatsPerMinute
                |> String.fromInt
    in
    Html.input
        [ Html.Attributes.type_ "number"
        , Html.Attributes.class "tempo-field"
        , Html.Attributes.value tempoAsString
        , Html.Attributes.min "20"
        , Html.Attributes.max "400"
        , Html.Events.onInput UserTypedIntoTempoField
        ]
        []


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
