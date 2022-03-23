module Project exposing
    ( Project, empty, new
    , addNote, removeNote
    , noteEvents, tempo
    )

{-|

@docs Project, empty, new

@docs addNote, removeNote

@docs noteEvents, tempo

-}

import Music.Event as Event
import Music.Note as Note
import Music.Tempo as Tempo


type Project
    = Project Details


type alias Details =
    { noteEvents : List (Event.Event Note.Note)
    , tempo : Tempo.Tempo
    }


new :
    { noteEvents : List (Event.Event Note.Note)
    , tempo : Tempo.Tempo
    }
    -> Project
new input =
    Project
        { noteEvents = input.noteEvents
        , tempo = input.tempo
        }


empty : Project
empty =
    Project
        { noteEvents = []
        , tempo = Tempo.quarterNotesPerMinute 120
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


tempo : Project -> Int
tempo (Project project) =
    Tempo.toSerial project.tempo
        |> .beatsPerMinute
