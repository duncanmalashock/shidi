module Main exposing (main)

import AssocSet as Set
import Browser
import Coordinate
import File.Download
import Html exposing (Html)
import Html.Attributes as Attr
import Html.Events
import Json.Encode
import Piano
import PianoRoll
import Ports
import Song


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
                    Coordinate.fromPixelsToGridCells coordinate

                gridRowIndex =
                    Coordinate.toGridCellsRecord newNote
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
                        (Coordinate.fromPixelsToGridCells coordinate)
                        model.song
              }
            , Cmd.none
            )

        PlaySong ->
            ( model, Ports.playSong (Song.toJson model.song) )

        SaveSong ->
            ( model, saveSong (Song.toJson model.song) )


saveSong : Json.Encode.Value -> Cmd Msg
saveSong value =
    File.Download.string "example.shidi" "text/json" (Json.Encode.encode 0 value)


view : Model -> { title : String, body : List (Html Msg) }
view model =
    { title = "App"
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
                            (Coordinate.fromPixelsToGridCells coordinate)

                    Nothing ->
                        Html.text ""
                , viewPlayButton
                , viewSaveButton
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


viewNotes : Song.Song -> Html Msg
viewNotes song =
    Song.toList song
        |> List.map (viewNote "deeppink")
        |> Html.div []


viewNote : String -> Coordinate.GridCells -> Html Msg
viewNote color coordinate =
    let
        { x, y } =
            Coordinate.fromGridCellsToPixels coordinate
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
