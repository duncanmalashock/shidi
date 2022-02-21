module Main exposing (main)

import Browser
import Html exposing (Html)
import Html.Attributes as Attr
import Html.Events
import Id exposing (Id)
import Keyboard
import List.Extra
import Music.Internal.ScaleStepper as ScaleStepper
import Music.Pitch
import Music.PitchClass
import Music.Scale
import Music.ScaleType
import Piano
import PianoRoll
import Ports
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
    , mousePosition : Maybe { x : Int, y : Int }
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
    , mousePosition = Nothing
    }


type Msg
    = ScaleRootClicked Id
    | ScaleTypeClicked Id
    | PitchClicked Id
    | KeyMsg Keyboard.Msg
    | NewStepCreated Id
    | DeleteStepButtonClicked Id
    | VoicingButtonClicked Id
    | NoteClicked Int
    | MouseMovedOverGrid { x : Int, y : Int }
    | MouseLeftGrid


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
                                (Step.pitches step
                                    |> List.head
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

        VoicingButtonClicked id ->
            model
                |> applyUserCommand (GenerateVoicingForStep id)

        NoteClicked noteNumber ->
            ( model, Ports.playNote noteNumber )

        MouseMovedOverGrid position ->
            ( { model | mousePosition = Just position }
            , Cmd.none
            )

        MouseLeftGrid ->
            ( { model | mousePosition = Nothing }
            , Cmd.none
            )


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
            Step.setPitches [ newPitch ] step
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


generateStepVoicing : Step -> List Step -> List Step
generateStepVoicing stepToUpdate steps =
    List.Extra.updateIf
        (\step ->
            Step.id step == Step.id stepToUpdate
        )
        (\step ->
            Step.generateVoicing step
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
    | GenerateVoicingForStep Id


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

        GenerateVoicingForStep id ->
            case getStep id model.steps of
                Just step ->
                    ( { model
                        | steps =
                            generateStepVoicing step model.steps
                      }
                    , Cmd.none
                    )

                _ ->
                    failWithError FailedToGenerateStepVoicing model


failWithError : Error -> Model -> ( Model, Cmd Msg )
failWithError error model =
    ( { model | lastError = Just error }
    , Cmd.none
    )


type Error
    = FailedToChangeStepPitch
    | FailedToChangeStepScaleRoot
    | FailedToChangeStepScaleType
    | FailedToGenerateStepVoicing


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
    Sub.map KeyMsg Keyboard.subscriptions
