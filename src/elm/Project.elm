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

import Music.Event as Event
import Music.Note as Note


type Project
    = Project Details


type alias Details =
    { noteEvents : List (Event.Event Note.Note)
    }


new : List (Event.Event Note.Note) -> Project
new notes_ =
    Project
        { noteEvents = notes_
        }


empty : Project
empty =
    Project
        { noteEvents = []
        }


addNote : Event.Event Note.Note -> Project -> Project
addNote noteToAdd (Project project) =
    Project
        { project
            | noteEvents = noteToAdd :: project.noteEvents
        }


removeNote : Event.Event Note.Note -> Project -> Project
removeNote noteToRemove (Project project) =
    let
        matches : Event.Event Note.Note -> Event.Event Note.Note -> Bool
        matches a b =
            (a.at == b.at) && (Note.pitch a.value == Note.pitch b.value)
    in
    Project
        { project
            | noteEvents =
                List.filter (\current -> not (matches noteToRemove current))
                    project.noteEvents
        }


noteEvents : Project -> List (Event.Event Note.Note)
noteEvents (Project project) =
    project.noteEvents
