module Step exposing
    ( Step, init
    , id, pitch, scaleType, scaleRoot
    , setPitch, setScaleType, setScaleRoot
    )

{-|

@docs Step, init

@docs id, pitch, scaleType, scaleRoot
@docs setPitch, setScaleType, setScaleRoot

-}

import Id
import Music.Pitch
import Music.PitchClass
import Music.Scale
import Music.ScaleType


type Step
    = Step Details


type alias Details =
    { scaleRoot : Music.PitchClass.PitchClass
    , scaleType : Music.ScaleType.ScaleType
    , pitches : List Music.Pitch.Pitch
    , id : Id.Id
    }


init : Id.Id -> Step
init id_ =
    Step
        { scaleRoot = Music.PitchClass.c
        , scaleType = Music.ScaleType.major
        , pitches = [ Music.Pitch.c4 ]
        , id = id_
        }


id : Step -> Id.Id
id (Step details) =
    details.id


scaleType : Step -> Music.ScaleType.ScaleType
scaleType (Step details) =
    details.scaleType


scaleRoot : Step -> Music.PitchClass.PitchClass
scaleRoot (Step details) =
    details.scaleRoot


pitch : Step -> Maybe Music.Pitch.Pitch
pitch (Step details) =
    List.head details.pitches


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


setPitch : Music.Pitch.Pitch -> Step -> Step
setPitch newPitch (Step details) =
    Step { details | pitches = [ newPitch ] }
