module Editor.Coordinate exposing
    ( MusicCoordinate
    , PixelCoordinate
    , fromMusicToPitchEvent
    , fromMusicToPixels
    , fromPixelsToMusic
    )

import Editor.Zoom
import Music
import Music.Duration
import Music.Pitch


type alias PixelCoordinate =
    { x : Int, y : Int }


type alias MusicCoordinate =
    { at : Music.Duration.Duration
    , pitch : Music.Pitch.Pitch
    }


pixelsXToStart : Editor.Zoom.Zoom -> Int -> Music.Duration.Duration
pixelsXToStart zoom x =
    Music.Duration.multiplyByInt (x // Editor.Zoom.cellSizeX zoom) Music.Duration.eighth


startToPixelsX : Editor.Zoom.Zoom -> Music.Duration.Duration -> Int
startToPixelsX zoom duration =
    Basics.round (Music.Duration.toFloat duration * 8) * Editor.Zoom.cellSizeX zoom


pixelsYToPitch : Editor.Zoom.Zoom -> Int -> Music.Pitch.Pitch
pixelsYToPitch zoom y =
    (131 - (y // Editor.Zoom.cellSizeY zoom))
        |> Music.Pitch.fromMIDINoteNumber


pitchToPixelsY : Editor.Zoom.Zoom -> Music.Pitch.Pitch -> Int
pitchToPixelsY zoom pitch =
    (131 - Music.Pitch.toMIDINoteNumber pitch)
        * Editor.Zoom.cellSizeY zoom


fromPixelsToMusic : Music.Measure -> Editor.Zoom.Zoom -> PixelCoordinate -> MusicCoordinate
fromPixelsToMusic measure zoom { x, y } =
    let
        xDuration : Music.Duration.Duration
        xDuration =
            Music.Duration.add measure.start (pixelsXToStart zoom x)
    in
    MusicCoordinate xDuration (pixelsYToPitch zoom y)


fromMusicToPixels : Editor.Zoom.Zoom -> MusicCoordinate -> PixelCoordinate
fromMusicToPixels zoom { at, pitch } =
    { x = startToPixelsX zoom at
    , y = pitchToPixelsY zoom pitch
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
