module Main exposing (main)

import Browser
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Id exposing (Id)
import Keyboard
import List.Extra
import Music.Internal.ScaleStepper as ScaleStepper
import Music.Pitch
import Music.PitchClass
import Music.Scale
import Music.ScaleType
import Random
import Step exposing (Step)
import UUID


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


init : () -> ( Model, Cmd Msg )
init flags =
    ( initialModel
    , newIdCmd NewStepCreated
    )


newIdCmd : (Id -> Msg) -> Cmd Msg
newIdCmd toMsg =
    Random.generate
        toMsg
        (UUID.generator
            |> Random.map (\uuid -> Id.id uuid)
        )


initialModel : Model
initialModel =
    { steps =
        []
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
    | NewStepCreated Id
    | DeleteStepButtonClicked Id


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
                                (Step.pitch step
                                    |> Maybe.withDefault Music.Pitch.c4
                                )
                                (Music.Scale.custom (Step.scaleRoot step)
                                    (Step.scaleType step)
                                )
                      }
                    , Cmd.none
                    )

                Nothing ->
                    ( model, Cmd.none )

        NewStepCreated id ->
            ( { model
                | steps =
                    model.steps ++ [ Step.init id ]
              }
            , Cmd.none
            )

        DeleteStepButtonClicked id ->
            model
                |> applyUserCommand (DeleteStep id)


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

                Keyboard.KeyDown (Keyboard.Character "n") ->
                    Just CreateNewStep

                _ ->
                    Nothing

        Nothing ->
            Nothing


type UserCommand
    = ChangeStepPitch Int
    | ChangeStepScaleType Music.ScaleType.ScaleType
    | ChangeStepScaleRoot Int
    | CreateNewStep
    | DeleteStep Id


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

        CreateNewStep ->
            ( model, newIdCmd NewStepCreated )

        DeleteStep id ->
            ( { model
                | steps =
                    List.filter
                        (\step ->
                            Step.id step /= id
                        )
                        model.steps
              }
            , Cmd.none
            )


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

        viewPitch : Music.Pitch.Pitch -> Html Msg
        viewPitch pitch =
            Html.button
                [ Html.Events.onClick (PitchClicked stepId)
                , if maybeSelection == Just (SelectionPitch stepId) then
                    Html.Attributes.style "color" "red"

                  else
                    Html.Attributes.style "color" "black"
                ]
                [ Html.text (Music.Pitch.toString pitch) ]

        viewDeleteButton : Html Msg
        viewDeleteButton =
            Html.button
                [ Html.Events.onClick (DeleteStepButtonClicked stepId)
                ]
                [ Html.text "Delete" ]
    in
    Html.div [] <|
        List.filterMap identity
            [ Just viewRoot
            , Just viewScaleType
            , Maybe.map viewPitch (Step.pitch step)
            , Just viewDeleteButton
            ]


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.map KeyMsg Keyboard.subscriptions
