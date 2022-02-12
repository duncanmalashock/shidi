module Main exposing (main)

import Browser
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Id exposing (Id)
import Keyboard
import List.Extra
import Music.Internal.Octave
import Music.Internal.ScaleStepper as ScaleStepper
import Music.Pitch
import Music.PitchClass
import Music.Scale
import Music.ScaleType
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
    , selection : Maybe Selection
    , scaleStepper : ScaleStepper.ScaleStepper
    , pressedKeys : List Keyboard.Key
    , lastError : Maybe Error
    }


type Selection
    = SelectionScaleRoot Id
    | SelectionScaleType Id
    | SelectionPitch Id


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
    , selection = Nothing
    , lastError = Nothing
    , scaleStepper =
        ScaleStepper.init Music.Pitch.c4 (Music.Scale.major Music.PitchClass.c)
    }


type Msg
    = ScaleRootClicked Id
    | ScaleTypeClicked Id
    | PitchClicked Id
    | KeyMsg Keyboard.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        KeyMsg keyMsg ->
            let
                ( pressedKeys, maybeKeyChange ) =
                    Keyboard.updateWithKeyChange
                        Keyboard.anyKeyOriginal
                        keyMsg
                        model.pressedKeys

                updatedModel : Model
                updatedModel =
                    { model | pressedKeys = pressedKeys }

                maybeUserCommand : Maybe UserCommand
                maybeUserCommand =
                    handleKeyEvent maybeKeyChange updatedModel
            in
            updatedModel
                |> applyMaybeUserCommand maybeUserCommand

        ScaleRootClicked id ->
            let
                maybeStep : Maybe Step
                maybeStep =
                    getStep id model.steps
            in
            case maybeStep of
                Just step ->
                    ( { model
                        | selection =
                            Just <|
                                SelectionScaleRoot id
                        , scaleStepper =
                            ScaleStepper.initChromatic
                                (Music.Pitch.fromPitchClassInOctave 4 (Step.scaleRoot step))
                      }
                    , Cmd.none
                    )

                Nothing ->
                    ( model, Cmd.none )

        ScaleTypeClicked id ->
            ( { model
                | selection =
                    Just <|
                        SelectionScaleType id
              }
            , Cmd.none
            )

        PitchClicked id ->
            let
                maybeStep : Maybe Step
                maybeStep =
                    getStep id model.steps
            in
            case maybeStep of
                Just step ->
                    ( { model
                        | selection =
                            Just <|
                                SelectionPitch id
                        , scaleStepper =
                            ScaleStepper.init
                                (Step.pitch step)
                                (Music.Scale.custom (Step.scaleRoot step)
                                    (Step.scaleType step)
                                )
                      }
                    , Cmd.none
                    )

                Nothing ->
                    ( model, Cmd.none )


getStep : Id -> List Step -> Maybe Step
getStep id steps =
    List.Extra.find
        (\step ->
            Id.match id (Step.id step)
        )
        steps


updateStepPitch : Id -> Music.Pitch.Pitch -> List Step -> List Step
updateStepPitch id newPitch steps =
    List.Extra.updateIf
        (\step ->
            Step.id step == id
        )
        (\step ->
            Step.setPitch newPitch step
        )
        steps


updateStepScaleRoot : Id -> Music.PitchClass.PitchClass -> List Step -> List Step
updateStepScaleRoot id newRoot steps =
    List.Extra.updateIf
        (\step ->
            Step.id step == id
        )
        (\step ->
            Step.setScaleRoot newRoot step
        )
        steps


updateStepScaleType : Music.ScaleType.ScaleType -> Id -> List Step -> List Step
updateStepScaleType scaleType selectedId steps =
    List.Extra.updateIf
        (\step ->
            Step.id step == selectedId
        )
        (\step ->
            Step.setScaleType scaleType step
        )
        steps


handleKeyEvent : Maybe Keyboard.KeyChange -> Model -> Maybe UserCommand
handleKeyEvent maybeKeyChange model =
    case maybeKeyChange of
        Just key ->
            case key of
                Keyboard.KeyDown Keyboard.ArrowUp ->
                    case model.selection of
                        Just (SelectionPitch _) ->
                            Just (ChangeStepPitch 1)

                        Just (SelectionScaleType _) ->
                            Just (ChangeStepScaleType Music.ScaleType.major)

                        Just (SelectionScaleRoot _) ->
                            Just (ChangeStepScaleRoot 1)

                        _ ->
                            Nothing

                Keyboard.KeyDown Keyboard.ArrowDown ->
                    case model.selection of
                        Just (SelectionPitch _) ->
                            Just (ChangeStepPitch -1)

                        Just (SelectionScaleType _) ->
                            Just (ChangeStepScaleType Music.ScaleType.minor)

                        Just (SelectionScaleRoot _) ->
                            Just (ChangeStepScaleRoot -1)

                        _ ->
                            Nothing

                _ ->
                    Nothing

        Nothing ->
            Nothing


type UserCommand
    = ChangeStepPitch Int
    | ChangeStepScaleType Music.ScaleType.ScaleType
    | ChangeStepScaleRoot Int


applyMaybeUserCommand : Maybe UserCommand -> Model -> ( Model, Cmd Msg )
applyMaybeUserCommand maybeUserCommand model =
    Maybe.map
        (\userCommand ->
            applyUserCommand userCommand model
        )
        maybeUserCommand
        |> Maybe.withDefault ( model, Cmd.none )


applyUserCommand : UserCommand -> Model -> ( Model, Cmd Msg )
applyUserCommand userCommand model =
    case userCommand of
        ChangeStepPitch stepsForChange ->
            case model.selection of
                Just (SelectionPitch selectedId) ->
                    let
                        updatedScaleStepper =
                            ScaleStepper.step stepsForChange model.scaleStepper
                    in
                    ( { model
                        | scaleStepper = updatedScaleStepper
                        , steps =
                            updateStepPitch selectedId
                                (ScaleStepper.pitch updatedScaleStepper)
                                model.steps
                      }
                    , Cmd.none
                    )

                _ ->
                    failWithError FailedToChangeStepPitch model

        ChangeStepScaleType scaleType ->
            case model.selection of
                Just (SelectionScaleType selectedId) ->
                    ( { model
                        | steps =
                            updateStepScaleType scaleType selectedId model.steps
                      }
                    , Cmd.none
                    )

                _ ->
                    failWithError FailedToChangeStepScaleType model

        ChangeStepScaleRoot stepsForChange ->
            case model.selection of
                Just (SelectionScaleRoot selectedId) ->
                    let
                        updatedScaleStepper =
                            ScaleStepper.step stepsForChange model.scaleStepper
                    in
                    ( { model
                        | scaleStepper = updatedScaleStepper
                        , steps =
                            updateStepScaleRoot selectedId
                                (ScaleStepper.pitch updatedScaleStepper
                                    |> Music.PitchClass.fromPitch
                                )
                                model.steps
                      }
                    , Cmd.none
                    )

                _ ->
                    failWithError FailedToChangeStepScaleRoot model


failWithError : Error -> Model -> ( Model, Cmd Msg )
failWithError error model =
    ( { model | lastError = Just error }
    , Cmd.none
    )


type Error
    = FailedToChangeStepPitch
    | FailedToChangeStepScaleRoot
    | FailedToChangeStepScaleType


view : Model -> { title : String, body : List (Html Msg) }
view model =
    { title = "App"
    , body =
        List.map
            (\step ->
                viewStep step model.selection
            )
            model.steps
    }


viewStep : Step -> Maybe Selection -> Html Msg
viewStep step maybeSelection =
    let
        stepId =
            Step.id step

        viewRoot : Html Msg
        viewRoot =
            Html.button
                [ Html.Events.onClick (ScaleRootClicked stepId)
                , if maybeSelection == Just (SelectionScaleRoot stepId) then
                    Html.Attributes.style "color" "red"

                  else
                    Html.Attributes.style "color" "black"
                ]
                [ Html.text (Music.PitchClass.toString (Step.scaleRoot step))
                ]

        viewScaleType : Html Msg
        viewScaleType =
            Html.button
                [ Html.Events.onClick (ScaleTypeClicked stepId)
                , if maybeSelection == Just (SelectionScaleType stepId) then
                    Html.Attributes.style "color" "red"

                  else
                    Html.Attributes.style "color" "black"
                ]
                [ Html.text
                    (Music.ScaleType.name (Step.scaleType step)
                        |> Maybe.withDefault ""
                    )
                ]

        viewPitch : Html Msg
        viewPitch =
            Html.button
                [ Html.Events.onClick (PitchClicked stepId)
                , if maybeSelection == Just (SelectionPitch stepId) then
                    Html.Attributes.style "color" "red"

                  else
                    Html.Attributes.style "color" "black"
                ]
                [ Html.text (Music.Pitch.toString (Step.pitch step)) ]
    in
    Html.div []
        [ viewRoot
        , viewScaleType
        , viewPitch
        ]


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.map KeyMsg Keyboard.subscriptions
