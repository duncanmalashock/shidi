module Step exposing
    ( Step
    , init
    , pitch
    , scaleRoot
    , scaleType
    , setScaleRoot
    , step
    )

import Music.Internal.ScaleStepper as ScaleStepper
import Music.Pitch
import Music.PitchClass
import Music.Scale
import Music.ScaleType


type Step
    = Step Details


type alias Details =
    { scaleRoot : Music.PitchClass.PitchClass
    , scaleType : Music.ScaleType.ScaleType
    , stepper : ScaleStepper.ScaleStepper
    }


init : Step
init =
    Step
        { scaleRoot = Music.PitchClass.c
        , scaleType = Music.ScaleType.major
        , stepper =
            --ScaleStepper.initChromatic Music.Pitch.c4
            ScaleStepper.init Music.Pitch.c4 (Music.Scale.major Music.PitchClass.c)
        }


scaleType : Step -> Music.ScaleType.ScaleType
scaleType (Step details) =
    details.scaleType


scaleRoot : Step -> Music.PitchClass.PitchClass
scaleRoot (Step details) =
    details.scaleRoot


pitch : Step -> Music.Pitch.Pitch
pitch (Step details) =
    ScaleStepper.currentPitch details.stepper


setScaleType : Music.ScaleType.ScaleType -> Step -> Step
setScaleType newScaleType (Step details) =
    Step
        { details
            | scaleType = newScaleType
        }


setScaleRoot : Music.PitchClass.PitchClass -> Step -> Step
setScaleRoot newPitchClass (Step details) =
    Step
        { details
            | scaleRoot = newPitchClass
        }


scale : Step -> Music.Scale.Scale
scale (Step details) =
    Music.Scale.custom details.scaleRoot details.scaleType


step : Int -> Step -> Step
step steps (Step details) =
    Step { details | stepper = ScaleStepper.step steps details.stepper }
