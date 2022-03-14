module Main exposing (main)

import Browser
import Browser.Dom
import Coordinate
import File
import File.Select
import File.Shidi
import Html exposing (Html)
import Html.Attributes as Attr
import Html.Events
import Json.Decode
import Music
import Music.Key
import Music.Meter
import Music.Tempo
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
    , music : Music.Music
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
    , music =
        Music.new
            { key = Music.Key.c
            , meter = Music.Meter.fourFour
            , tempo = Music.Tempo.allegro
            }
    }


type Msg
    = MouseMovedOverGrid Coordinate.Pixels
    | MouseLeftGrid
    | PianoNoteClicked Int
    | NoteAdded Coordinate.Pixels
    | NoteRemoved Coordinate.Pixels
    | PlaySong
    | SaveSong
    | LoadSong
    | SongFileSelected File.File
    | SongFileLoaded (Result Json.Decode.Error Song.Song)
    | SaveSongConfirmed
    | FileNameInputFocused (Result Browser.Dom.Error ())
    | TypedIntoNameField String
    | DismissedSaveModal


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        MouseMovedOverGrid position ->
            ( { model | mousePosition = Just position }
            , Cmd.none
            )

        MouseLeftGrid ->
            ( { model | mousePosition = Nothing }
            , Cmd.none
            )

        PianoNoteClicked note ->
            ( model, Ports.playNote note )

        NoteAdded coordinate ->
            let
                newNote : Coordinate.PianoRoll
                newNote =
                    Coordinate.fromPixelsToPianoRoll coordinate

                gridRowIndex =
                    Coordinate.toPianoRollRecord newNote
            in
            ( { model
                | song =
                    Song.addNote newNote model.song
              }
            , Ports.playNote (131 - gridRowIndex.y)
            )

        NoteRemoved coordinate ->
            ( { model
                | song =
                    Song.removeNote
                        (Coordinate.fromPixelsToPianoRoll coordinate)
                        model.song
              }
            , Cmd.none
            )

        PlaySong ->
            ( model, Ports.playSong model.song )

        SaveSong ->
            ( { model
                | showSaveModal = True
              }
            , Task.attempt FileNameInputFocused (Browser.Dom.focus "filename-input")
            )

        FileNameInputFocused _ ->
            ( model, Cmd.none )

        LoadSong ->
            ( model, File.Select.file [ "text/shidi" ] SongFileSelected )

        SongFileSelected file ->
            ( { model
                | fileName =
                    File.name file
                        |> String.dropRight (String.length ".shidi")
              }
            , File.Shidi.load SongFileLoaded file
            )

        SongFileLoaded result ->
            case result of
                Ok song ->
                    ( { model | song = song }, Cmd.none )

                Err error ->
                    ( model, Cmd.none )

        SaveSongConfirmed ->
            ( { model | showSaveModal = False }
            , File.Shidi.save model.fileName model.song
            )

        TypedIntoNameField newFileName ->
            ( { model | fileName = newFileName }, Cmd.none )

        DismissedSaveModal ->
            ( { model | showSaveModal = False }
            , Cmd.none
            )


view : Model -> { title : String, body : List (Html Msg) }
view model =
    { title = "shidi"
    , body =
        [ Html.div [ Attr.class "row" ]
            [ Piano.view PianoNoteClicked
            , Html.div
                [ Attr.class "piano-roll__wrapper" ]
                [ PianoRoll.view
                    { onMouseMove = MouseMovedOverGrid
                    , onMouseLeave = MouseLeftGrid
                    , onLeftClick = NoteAdded
                    , onRightClick = NoteRemoved
                    }
                , viewNotes model.song
                , case model.mousePosition of
                    Just coordinate ->
                        viewNote "mediumseagreen"
                            (Coordinate.fromPixelsToPianoRoll coordinate)

                    Nothing ->
                        Html.text ""
                , viewPlayButton
                , viewSaveButton
                , viewLoadButton
                ]
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
                , Html.Events.onClick DismissedSaveModal
                ]
                []
            , Html.form
                [ Attr.class "modal"
                , Html.Events.onSubmit SaveSongConfirmed
                ]
                [ Html.input
                    [ Attr.type_ "text"
                    , Html.Events.onInput TypedIntoNameField
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
        , Html.Events.onClick PlaySong
        ]
        [ Html.text "Play" ]


viewSaveButton : Html Msg
viewSaveButton =
    Html.button
        [ Attr.class "save-button"
        , Html.Events.onClick SaveSong
        ]
        [ Html.text "Save" ]


viewLoadButton : Html Msg
viewLoadButton =
    Html.button
        [ Attr.class "load-button"
        , Html.Events.onClick LoadSong
        ]
        [ Html.text "Load" ]


viewNotes : Song.Song -> Html Msg
viewNotes song =
    Song.notes song
        |> List.map (viewNote "deeppink")
        |> Html.div []


viewNote : String -> Coordinate.PianoRoll -> Html Msg
viewNote color coordinate =
    let
        { x, y } =
            Coordinate.fromPianoRollToPixels coordinate
                |> Coordinate.toPixelsRecord
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
