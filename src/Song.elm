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

import Music


type Song
    = Song Details


type alias Details =
    { noteEvents : List Music.NoteEvent
    }


new : List Music.NoteEvent -> Song
new notes_ =
    Song
        { noteEvents = notes_
        }


empty : Song
empty =
    Song
        { noteEvents = []
        }


addNote : Music.NoteEvent -> Song -> Song
addNote noteToAdd (Song song) =
    Song
        { song
            | noteEvents = noteToAdd :: song.noteEvents
        }


removeNote : Music.NoteEvent -> Song -> Song
removeNote noteToRemove (Song song) =
    Song
        { song
            | noteEvents =
                List.filter (\current -> current /= noteToRemove)
                    song.noteEvents
        }


notes : Song -> List Music.NoteEvent
notes (Song song) =
    song.noteEvents
