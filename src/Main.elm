module Main exposing (main)

import Browser
import Html exposing (Html)
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


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )


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
    Sub.none
