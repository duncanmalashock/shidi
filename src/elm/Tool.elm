module Tool exposing (Tool(..), addNote)

import Music.Duration


type Tool
    = AddNote Music.Duration.Duration


addNote : Music.Duration.Duration -> Tool
addNote duration =
    AddNote duration
