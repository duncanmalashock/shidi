module Main exposing (main)

import AssocSet as Set
import Browser
import Coordinate
import Html exposing (Html)
import Html.Attributes as Attr
import Piano
import PianoRoll


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
    , notes : Set.Set Coordinate.GridCells
    }


init : () -> ( Model, Cmd Msg )
init flags =
    ( initialModel
    , Cmd.none
    )


initialModel : Model
initialModel =
    { mousePosition = Nothing
    , notes = Set.empty
    }


type Msg
    = MouseMovedOverGrid Coordinate.Pixels
    | MouseLeftGrid
    | PianoNoteClicked Int
    | NoteAdded Coordinate.Pixels
    | NoteRemoved Coordinate.Pixels


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

        PianoNoteClicked int ->
            ( model, Cmd.none )

        NoteAdded coordinate ->
            ( { model
                | notes =
                    Set.insert
                        (Coordinate.fromPixelsToGridCells coordinate)
                        model.notes
              }
            , Cmd.none
            )

        NoteRemoved coordinate ->
            ( { model
                | notes =
                    Set.remove
                        (Coordinate.fromPixelsToGridCells coordinate)
                        model.notes
              }
            , Cmd.none
            )


view : Model -> { title : String, body : List (Html Msg) }
view model =
    { title = "App"
    , body =
        [ Html.div [ Attr.class "row" ]
            [ viewPiano
            , Html.div
                [ Attr.class "piano-roll__wrapper" ]
                [ PianoRoll.view
                    { onMouseMove = MouseMovedOverGrid
                    , onMouseLeave = MouseLeftGrid
                    , onLeftClick = NoteAdded
                    , onRightClick = NoteRemoved
                    }
                , viewNotes model.notes
                , case model.mousePosition of
                    Just coordinate ->
                        viewNote "mediumseagreen"
                            (Coordinate.fromPixelsToGridCells coordinate)

                    Nothing ->
                        Html.text ""
                ]
            ]
        ]
    }


viewNotes : Set.Set Coordinate.GridCells -> Html Msg
viewNotes coordinateSet =
    Set.toList coordinateSet
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


viewPiano : Html Msg
viewPiano =
    List.range 0 9
        |> List.reverse
        |> List.map (Piano.viewOctave PianoNoteClicked)
        |> Html.div []


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
