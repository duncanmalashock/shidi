module Main exposing (main)

import Browser
import Coordinate
import File
import File.Download
import File.Select
import Html exposing (Html)
import Html.Attributes as Attr
import Html.Events
import Json.Encode
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
    }


init : () -> ( Model, Cmd Msg )
init flags =
    ( initialModel
    , Cmd.none
    )


initialModel : Model
initialModel =
    { mousePosition = Nothing
    , song = Song.new
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
    | SongFileLoaded String


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
            ( model, Ports.playSong (Song.toJson model.song) )

        SaveSong ->
            ( model, saveSong (Song.toJson model.song) )

        LoadSong ->
            ( model, File.Select.file [ "text/shidi" ] SongFileSelected )

        SongFileSelected file ->
            ( model, Task.perform SongFileLoaded (File.toString file) )

        SongFileLoaded jsonString ->
            case Song.fromJsonString jsonString of
                Ok song ->
                    ( { model | song = song }, Cmd.none )

                Err error ->
                    ( model, Cmd.none )


saveSong : Json.Encode.Value -> Cmd Msg
saveSong value =
    File.Download.string "example.shidi" "text/shidi" (Json.Encode.encode 0 value)


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
        ]
    }


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
    Song.toList song
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
