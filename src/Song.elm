module Song exposing
    ( Song, empty, new
    , addNote, removeNote
    , notes
    )

{-|

@docs Song, empty, new

@docs addNote, removeNote

@docs notes

-}

import AssocSet as Set
import Coordinate
import Music
import Music.Duration
import Music.Note
import Music.Pitch


type Song
    = Song Details


type alias Details =
    { noteEvents : List Music.NoteEvent
    }


pianoRollCoordinateToNoteEvent : Coordinate.PianoRoll -> Music.NoteEvent
pianoRollCoordinateToNoteEvent coordinate =
    let
        { x, y } =
            Coordinate.toPianoRollRecord coordinate
    in
    { at = Music.Duration.multiplyByInt x Music.Duration.quarter
    , value = Music.Note.quarter (Music.Pitch.fromMIDINoteNumber y)
    }


noteEventToPianoRollCoordinate : Music.NoteEvent -> Coordinate.PianoRoll
noteEventToPianoRollCoordinate { at, value } =
    let
        x : Int
        x =
            Basics.round (Music.Duration.toFloat at * 4)

        y : Int
        y =
            Music.Note.pitch value
                |> Music.Pitch.toMIDINoteNumber
    in
    Coordinate.pianoRoll x y


new : List Coordinate.PianoRoll -> Song
new notes_ =
    Song
        { noteEvents =
            notes_
                |> List.map pianoRollCoordinateToNoteEvent
        }


empty : Song
empty =
    Song
        { noteEvents = []
        }


addNote : Coordinate.PianoRoll -> Song -> Song
addNote newNote (Song song) =
    let
        noteToAdd : Music.NoteEvent
        noteToAdd =
            pianoRollCoordinateToNoteEvent newNote
    in
    Song
        { song
            | noteEvents = noteToAdd :: song.noteEvents
        }


removeNote : Coordinate.PianoRoll -> Song -> Song
removeNote newNote (Song song) =
    let
        noteToRemove : Music.NoteEvent
        noteToRemove =
            pianoRollCoordinateToNoteEvent newNote
    in
    Song
        { song
            | noteEvents =
                List.filter (\current -> current /= noteToRemove)
                    song.noteEvents
        }


notes : Song -> List Coordinate.PianoRoll
notes (Song song) =
    List.map noteEventToPianoRollCoordinate song.noteEvents
