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


type Song
    = Song Details


type alias Details =
    { noteEvents : Set.Set Coordinate.PianoRoll
    }


new : List Coordinate.PianoRoll -> Song
new notes_ =
    Song
        { noteEvents = Set.fromList notes_
        }


empty : Song
empty =
    Song
        { noteEvents = Set.empty
        }


addNote : Coordinate.PianoRoll -> Song -> Song
addNote newNote (Song song) =
    Song
        { song
            | noteEvents = Set.insert newNote song.noteEvents
        }


removeNote : Coordinate.PianoRoll -> Song -> Song
removeNote newNote (Song song) =
    Song
        { song
            | noteEvents = Set.remove newNote song.noteEvents
        }


notes : Song -> List Coordinate.PianoRoll
notes (Song song) =
    Set.toList song.noteEvents
