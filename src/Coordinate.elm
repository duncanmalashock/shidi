module Coordinate exposing
    ( Pixels, pixels
    , pixelsXY
    , Music
    , fromMusicToNoteEvent, fromMusicToPixels, fromNoteEventToMusic, fromPixelsToMusic
    )

{-|

@docs Pixels, pixels

@docs pixelsXY

@docs Music

-}

import Music
import Music.Duration
import Music.Note
import Music.Pitch


type Pixels
    = Pixels { x : Int, y : Int }


type Music
    = Music Music.Duration.Duration Music.Pitch.Pitch


pixelsXY : Pixels -> { x : Int, y : Int }
pixelsXY (Pixels coordinateRecord) =
    coordinateRecord


pixels : Int -> Int -> Pixels
pixels x y =
    Pixels { x = x, y = y }


cellSizeX : Int
cellSizeX =
    21


cellSizeY : Int
cellSizeY =
    21


pixelsXToDuration : Int -> Music.Duration.Duration
pixelsXToDuration x =
    Music.Duration.multiplyByInt (x // cellSizeX) Music.Duration.quarter


durationToPixelsX : Music.Duration.Duration -> Int
durationToPixelsX duration =
    Basics.round (Music.Duration.toFloat duration * 4) * cellSizeX


pixelsYToPitch : Int -> Music.Pitch.Pitch
pixelsYToPitch y =
    (131 - (y // cellSizeY))
        |> Music.Pitch.fromMIDINoteNumber


pitchToPixelsY : Music.Pitch.Pitch -> Int
pitchToPixelsY pitch =
    (131 - Music.Pitch.toMIDINoteNumber pitch)
        * cellSizeY


fromPixelsToMusic : Pixels -> Music
fromPixelsToMusic (Pixels { x, y }) =
    Music (pixelsXToDuration x) (pixelsYToPitch y)


fromMusicToPixels : Music -> Pixels
fromMusicToPixels (Music duration pitch) =
    Pixels
        { x = durationToPixelsX duration
        , y = pitchToPixelsY pitch
        }


fromMusicToNoteEvent : Music -> Music.NoteEvent
fromMusicToNoteEvent (Music duration pitch) =
    { at = duration
    , value = Music.Note.quarter pitch
    }


fromNoteEventToMusic : Music.NoteEvent -> Music
fromNoteEventToMusic noteEvent =
    Music noteEvent.at (Music.Note.pitch noteEvent.value)
