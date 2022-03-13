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

import AssocSet
import Coordinate


type Song
    = Song (AssocSet.Set Coordinate.PianoRoll)


new : List Coordinate.PianoRoll -> Song
new notes_ =
    Song (AssocSet.fromList notes_)


empty : Song
empty =
    Song AssocSet.empty


addNote : Coordinate.PianoRoll -> Song -> Song
addNote newNote (Song song) =
    Song (AssocSet.insert newNote song)


removeNote : Coordinate.PianoRoll -> Song -> Song
removeNote newNote (Song song) =
    Song (AssocSet.remove newNote song)


notes : Song -> List Coordinate.PianoRoll
notes (Song song) =
    AssocSet.toList song
