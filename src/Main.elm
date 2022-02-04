module Main exposing (main)

import Browser
import Browser.Events
import Html exposing (Html)
import Json.Decode
import Keyboard.Event
import Music.Pitch
import Music.PitchClass
import Step exposing (Step)


main : Program () Model Msg
main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type alias Model =
    { steps : List Step
    }


init : () -> ( Model, Cmd msg )
init flags =
    ( initialModel
    , Cmd.none
    )


initialModel : Model
initialModel =
    { steps =
        [ Step.init
        ]
    }


type Msg
    = NoOp
    | KeyEventReceived KeyEvent


type KeyEvent
    = KeyUp
    | KeyDown


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        KeyEventReceived keyEvent ->
            ( { model
                | steps = updateStep 0 keyEvent model.steps
              }
            , Cmd.none
            )


updateStep : Int -> KeyEvent -> List Step -> List Step
updateStep index keyEvent steps =
    List.map
        (\step ->
            step
                |> Step.setScaleRoot
                    Music.PitchClass.f
        )
        steps


view : Model -> { title : String, body : List (Html Msg) }
view model =
    { title = "App"
    , body =
        List.map viewStep model.steps
    }


viewStep : Step -> Html Msg
viewStep step =
    Html.div []
        [ Html.text (Music.PitchClass.toString (Step.scaleRoot step))
        , Html.text (Music.Pitch.toString (Step.pitch step))
        ]


subscriptions : Model -> Sub Msg
subscriptions model =
    Browser.Events.onKeyDown
        (Keyboard.Event.decodeKeyboardEvent
            |> Json.Decode.andThen decodeKeyEvent
        )


decodeKeyEvent : Keyboard.Event.KeyboardEvent -> Json.Decode.Decoder Msg
decodeKeyEvent keyboardEvent =
    let
        toKeyEvent : Maybe KeyEvent
        toKeyEvent =
            if keyboardEvent.key == Just "ArrowUp" then
                Just KeyUp

            else if keyboardEvent.key == Just "ArrowDown" then
                Just KeyDown

            else
                Nothing
    in
    toKeyEvent
        |> Maybe.map
            (\keyEvent ->
                Json.Decode.succeed (KeyEventReceived keyEvent)
            )
        |> Maybe.withDefault
            (Json.Decode.fail "Not a relevant key event")
