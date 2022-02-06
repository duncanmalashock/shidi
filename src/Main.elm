module Main exposing (main)

import Browser
import Browser.Events
import Html exposing (Html)
import Json.Decode
import Keyboard
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
    , pressedKeys : List Keyboard.Key
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
    , pressedKeys = []
    }


type Msg
    = NoOp
    | KeyMsg Keyboard.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        KeyMsg keyMsg ->
            let
                ( pressedKeys, maybeKeyChange ) =
                    Keyboard.updateWithKeyChange
                        Keyboard.anyKeyOriginal
                        keyMsg
                        model.pressedKeys
            in
            { model | pressedKeys = pressedKeys }
                |> handleKeyChange maybeKeyChange


handleKeyChange : Maybe Keyboard.KeyChange -> Model -> ( Model, Cmd msg )
handleKeyChange maybeKeyChange model =
    case maybeKeyChange of
        Just key ->
            case key of
                Keyboard.KeyDown Keyboard.ArrowDown ->
                    ( { model
                        | steps =
                            updateStep 0 PitchChangeDown model.steps
                      }
                    , Cmd.none
                    )

                Keyboard.KeyDown Keyboard.ArrowUp ->
                    ( { model
                        | steps =
                            updateStep 0 PitchChangeUp model.steps
                      }
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )

        Nothing ->
            ( model, Cmd.none )


type PitchChange
    = PitchChangeUp
    | PitchChangeDown


updateStep : Int -> PitchChange -> List Step -> List Step
updateStep index pitchChange steps =
    List.map
        (\step ->
            step
                |> Step.setScaleRoot
                    (case pitchChange of
                        PitchChangeUp ->
                            Music.PitchClass.d

                        PitchChangeDown ->
                            Music.PitchClass.b
                    )
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
    Sub.map KeyMsg Keyboard.subscriptions
