module Editor.Coordinate exposing
    ( Music
    , Pixel
    , fromMusicToPixels
    , fromPixelsToMusic
    )

import Music
import Music.Duration
import Music.Pitch
import Zoom


type alias Pixel =
    { x : Int, y : Int }


type alias Music =
    { at : Music.Duration.Duration
    , pitch : Music.Pitch.Pitch
    }


pixelsXToStart : Zoom.Zoom -> Int -> Music.Duration.Duration
pixelsXToStart zoom x =
    Music.Duration.multiplyByInt (x // Zoom.cellSizeX zoom) Music.Duration.eighth


startToPixelsX : Zoom.Zoom -> Music.Duration.Duration -> Int
startToPixelsX zoom duration =
    Basics.round (Music.Duration.toFloat duration * 8) * Zoom.cellSizeX zoom


pixelsYToPitch : Zoom.Zoom -> Int -> Music.Pitch.Pitch
pixelsYToPitch zoom y =
    (131 - (y // Zoom.cellSizeY zoom))
        |> Music.Pitch.fromMIDINoteNumber


pitchToPixelsY : Zoom.Zoom -> Music.Pitch.Pitch -> Int
pitchToPixelsY zoom pitch =
    (131 - Music.Pitch.toMIDINoteNumber pitch)
        * Zoom.cellSizeY zoom


fromPixelsToMusic : Music.Measure -> Zoom.Zoom -> Pixel -> Music
fromPixelsToMusic measure zoom { x, y } =
    let
        xDuration : Music.Duration.Duration
        xDuration =
            Music.Duration.add measure.start (pixelsXToStart zoom x)
    in
    Music xDuration (pixelsYToPitch zoom y)


fromMusicToPixels : Zoom.Zoom -> Music -> Pixel
fromMusicToPixels zoom { at, pitch } =
    { x = startToPixelsX zoom at
    , y = pitchToPixelsY zoom pitch
    }
