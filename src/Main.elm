module Main exposing (main)

import Browser
import Browser.Dom
import File
import File.Load
import File.Save
import File.Select
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Json.Decode
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
    { project : Project.Project
    , fileName : String
    , showSaveModal : Bool
    , pianoRoll : PianoRoll.Model
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
    , pianoRoll = PianoRoll.init
    }


type Msg
    = -- Piano keys
      UserClickedPianoKey Int
      -- Piano roll
    | PianoRollMsg PianoRoll.Msg
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
        UserClickedPianoKey note ->
            ( model, Ports.playNote note )

        PianoRollMsg pianoRollMsg ->
            let
                ( pianoRoll, maybeOutMsg ) =
                    PianoRoll.update pianoRollMsg model.pianoRoll

                { cmd, updatedProject } =
                    case maybeOutMsg of
                        Just (PianoRoll.AddNoteEvent noteEvent midiPitch) ->
                            { cmd = Ports.playNote midiPitch
                            , updatedProject = Project.addNote noteEvent model.project
                            }

                        Just (PianoRoll.RemoveNoteEvent noteEvent) ->
                            { cmd = Cmd.none
                            , updatedProject = Project.removeNote noteEvent model.project
                            }

                        Nothing ->
                            { cmd = Cmd.none
                            , updatedProject = model.project
                            }
            in
            ( { model
                | pianoRoll = pianoRoll
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
        [ Html.div [ Html.Attributes.class "row" ]
            [ Piano.view UserClickedPianoKey
            , PianoRoll.view
                { project = model.project
                , model = model.pianoRoll
                , toMsg = PianoRollMsg
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
                [ Html.Attributes.class "modal-dismiss"
                , Html.Events.onClick UserDismissedSaveModal
                ]
                []
            , Html.form
                [ Html.Attributes.class "modal"
                , Html.Events.onSubmit UserClickedModalSaveButton
                ]
                [ Html.input
                    [ Html.Attributes.type_ "text"
                    , Html.Events.onInput UserTypedIntoNameField
                    , Html.Attributes.value model.fileName
                    , Html.Attributes.id "filename-input"
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
        [ Html.Attributes.class "play-button"
        , Html.Events.onClick UserClickedPlayButton
        ]
        [ Html.text "Play" ]


viewSaveButton : Html Msg
viewSaveButton =
    Html.button
        [ Html.Attributes.class "save-button"
        , Html.Events.onClick UserClickedSaveButton
        ]
        [ Html.text "Save" ]


viewLoadButton : Html Msg
viewLoadButton =
    Html.button
        [ Html.Attributes.class "load-button"
        , Html.Events.onClick UserClickedLoadButton
        ]
        [ Html.text "Load" ]


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
