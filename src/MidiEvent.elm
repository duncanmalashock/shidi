module MidiEvent exposing (MidiEvent, fromNoteEvent, new, toNoteEvent)

import Music
import Music.Duration
import Music.Note
import Music.Pitch


type alias MidiEvent =
    { pitch : Int
    , start : Int
    }


new : { pitch : Int, start : Int } -> MidiEvent
new { pitch, start } =
    { pitch = pitch
    , start = start
    }


fromNoteEvent : Music.NoteEvent -> MidiEvent
fromNoteEvent noteEvent =
    let
        midiNoteNumber : Int
        midiNoteNumber =
            Music.Note.pitch noteEvent.value
                |> Music.Pitch.toMIDINoteNumber

        noteStartTime : Int
        noteStartTime =
            Music.Duration.toFloat noteEvent.at
                * 4
                |> Basics.round
    in
    { pitch = midiNoteNumber
    , start = noteStartTime
    }


toNoteEvent : MidiEvent -> Music.NoteEvent
toNoteEvent { pitch, start } =
    { at = Music.Duration.multiplyByInt start Music.Duration.quarter
    , value =
        Music.Note.note (Music.Pitch.fromMIDINoteNumber pitch)
            Music.Duration.quarter
    }
