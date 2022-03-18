module Project exposing
    ( Project, empty, new
    , addNote, removeNote
    , noteEvents
    )

{-|

@docs Project, empty, new

@docs addNote, removeNote

@docs noteEvents

-}

import Music


type Project
    = Project Details


type alias Details =
    { noteEvents : List Music.NoteEvent
    }


new : List Music.NoteEvent -> Project
new notes_ =
    Project
        { noteEvents = notes_
        }


empty : Project
empty =
    Project
        { noteEvents = []
        }


addNote : Music.NoteEvent -> Project -> Project
addNote noteToAdd (Project project) =
    Project
        { project
            | noteEvents = noteToAdd :: project.noteEvents
        }


removeNote : Music.NoteEvent -> Project -> Project
removeNote noteToRemove (Project project) =
    Project
        { project
            | noteEvents =
                List.filter (\current -> current /= noteToRemove)
                    project.noteEvents
        }


noteEvents : Project -> List Music.NoteEvent
noteEvents (Project project) =
    project.noteEvents
