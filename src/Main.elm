module Main exposing (main)

import Browser
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
    { mousePosition : Maybe { x : Int, y : Int }
    }


init : () -> ( Model, Cmd Msg )
init flags =
    ( initialModel
    , Cmd.none
    )


initialModel : Model
initialModel =
    { mousePosition = Nothing
    }


type Msg
    = MouseMovedOverGrid { x : Int, y : Int }
    | MouseLeftGrid
    | NoteClicked Int


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

        NoteClicked int ->
            ( model, Cmd.none )


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
                    }
                , case model.mousePosition of
                    Just position ->
                        let
                            x =
                                position.x
                                    // 21
                                    * 21

                            y =
                                position.y
                                    // 21
                                    * 21
                        in
                        Html.div
                            [ Attr.class "note-preview"
                            , Attr.style "transform"
                                ("translate($x, $y)"
                                    |> String.replace "$x" (String.fromInt x ++ "px")
                                    |> String.replace "$y" (String.fromInt y ++ "px")
                                )
                            ]
                            []

                    Nothing ->
                        Html.text ""
                ]
            ]
        ]
    }


viewPiano : Html Msg
viewPiano =
    List.range 0 9
        |> List.reverse
        |> List.map (Piano.viewOctave NoteClicked)
        |> Html.div []


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
