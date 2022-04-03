module Project exposing
    ( Project, empty, new
    , noteEvents
    , addNote, removeNote
    , tempo
    , setInitialTempo
    )

{-|

@docs Project, empty, new

@docs noteEvents
@docs addNote, removeNote

@docs tempo
@docs setTempo

-}

import Music
import Music.Event as Event
import Music.Key as Key
import Music.Meter as Meter
import Music.Note as Note
import Music.Tempo as Tempo


type Project
    = Project Details


type alias Details =
    { music : Music.Music
    }


new :
    { noteEvents : List (Event.Event Note.Note)
    , tempo : Tempo.Tempo
    , meter : Meter.Meter
    , key : Key.Key
    }
    -> Project
new input =
    Project
        { music =
            Music.new
                { tempo = input.tempo
                , key = input.key
                , meter = input.meter
                , measures = defaultMeasureLength
                }
                |> Music.addNoteEvents input.noteEvents
        }


empty : Project
empty =
    Project
        { music =
            Music.new
                { tempo = defaultTempo
                , key = Key.c
                , meter = Meter.fourFour
                , measures = defaultMeasureLength
                }
        }


defaultTempo : Tempo.Tempo
defaultTempo =
    Tempo.quarterNotesPerMinute 120


defaultMeasureLength : Int
defaultMeasureLength =
    4


addNote : Event.Event Note.Note -> Project -> Project
addNote noteToAdd (Project project) =
    Project
        { project
            | music = Music.addNote noteToAdd project.music
        }


removeNote : Event.Event Note.Note -> Project -> Project
removeNote noteToRemove (Project project) =
    Project
        { project
            | music = Music.removeNote noteToRemove project.music
        }


setInitialTempo : Tempo.Tempo -> Project -> Project
setInitialTempo newTempo (Project project) =
    Project
        { project
            | music = Music.setInitialTempo newTempo project.music
        }


noteEvents : Project -> List (Event.Event Note.Note)
noteEvents (Project project) =
    Music.noteEvents project.music


tempo : Project -> Tempo.Tempo
tempo (Project project) =
    Music.tempoEvents project.music
        |> List.head
        |> Maybe.map .value
        |> Maybe.withDefault defaultTempo
