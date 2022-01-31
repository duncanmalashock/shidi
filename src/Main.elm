module Main exposing (main)

import Browser
import Html exposing (Html)


main : Program () Model Msg
main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type alias Model =
    {}


init : () -> ( Model, Cmd msg )
init flags =
    ( {}
    , Cmd.none
    )


type Msg
    = NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( {}, Cmd.none )


view : Model -> { title : String, body : List (Html Msg) }
view model =
    { title = "App"
    , body =
        [ Html.text "hey world" ]
    }


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
