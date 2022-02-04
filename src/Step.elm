module Step exposing
    ( Step
    , init
    , pitch
    , scaleRoot
    , scaleType
    )

import Music.Pitch
import Music.PitchClass
import Music.Scale
import Music.ScaleType


type Step
    = Step Details


type alias Details =
    { scaleRoot : Music.PitchClass.PitchClass
    , scaleType : Music.ScaleType.ScaleType
    , pitch : Music.Pitch.Pitch
    }


init : Step
init =
    Step
        { scaleRoot = Music.PitchClass.c
        , scaleType = Music.ScaleType.major
        , pitch = Music.Pitch.c4
        }


scaleType : Step -> Music.ScaleType.ScaleType
scaleType (Step details) =
    details.scaleType


scaleRoot : Step -> Music.PitchClass.PitchClass
scaleRoot (Step details) =
    details.scaleRoot


pitch : Step -> Music.Pitch.Pitch
pitch (Step details) =
    details.pitch


setScaleType : Music.ScaleType.ScaleType -> Step -> Step
setScaleType newScaleType (Step details) =
    Step
        { details
            | scaleType = newScaleType
        }


setScalePitchClass : Music.PitchClass.PitchClass -> Step -> Step
setScalePitchClass newPitchClass (Step details) =
    Step
        { details
            | scaleRoot = newPitchClass
        }


scale : Step -> Music.Scale.Scale
scale (Step details) =
    Music.Scale.custom details.scaleRoot details.scaleType
