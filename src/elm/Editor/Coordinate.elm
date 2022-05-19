module Editor.Coordinate exposing
    ( MusicCoordinate
    , PixelCoordinate
    , fromMusicToPitchEvent
    , fromMusicToPixels
    , fromPixelsToMusic
    )

import Editor.Scale
import Music
import Music.Duration
import Music.Pitch


type alias PixelCoordinate =
    { x : Int, y : Int }


type alias MusicCoordinate =
    { at : Music.Duration.Duration
    , pitch : Music.Pitch.Pitch
    }


pixelsXToStart : Editor.Scale.ScaleX -> Int -> Music.Duration.Duration
pixelsXToStart scaleX x =
    Music.Duration.multiplyByInt (x // Editor.Scale.cellSizeX scaleX) Music.Duration.eighth


startToPixelsX : Editor.Scale.ScaleX -> Music.Duration.Duration -> Int
startToPixelsX scaleX duration =
    Basics.round (Music.Duration.toFloat duration * 8) * Editor.Scale.cellSizeX scaleX


pixelsYToPitch : Editor.Scale.ScaleY -> Int -> Music.Pitch.Pitch
pixelsYToPitch scaleY y =
    (131 - (y // Editor.Scale.cellSizeY scaleY))
        |> Music.Pitch.fromMIDINoteNumber


pitchToPixelsY : Editor.Scale.ScaleY -> Music.Pitch.Pitch -> Int
pitchToPixelsY scaleY pitch =
    (131 - Music.Pitch.toMIDINoteNumber pitch)
        * Editor.Scale.cellSizeY scaleY


fromPixelsToMusic : Music.Measure -> Editor.Scale.ScaleX -> Editor.Scale.ScaleY -> PixelCoordinate -> MusicCoordinate
fromPixelsToMusic measure scaleX scaleY { x, y } =
    let
        xDuration : Music.Duration.Duration
        xDuration =
            Music.Duration.add measure.start (pixelsXToStart scaleX x)
    in
    MusicCoordinate xDuration (pixelsYToPitch scaleY y)


fromMusicToPixels : Editor.Scale.ScaleX -> Editor.Scale.ScaleY -> MusicCoordinate -> PixelCoordinate
fromMusicToPixels scaleX scaleY { at, pitch } =
    { x = startToPixelsX scaleX at
    , y = pitchToPixelsY scaleY pitch
    }


fromMusicToPitchEvent : MusicCoordinate -> PitchEvent
fromMusicToPitchEvent { at, pitch } =
    { at = at
    , value = pitch
    }


type alias PitchEvent =
    { at : Music.Duration.Duration
    , value : Music.Pitch.Pitch
    }
