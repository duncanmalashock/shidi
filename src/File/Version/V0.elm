module File.Version.V0 exposing (decoder, encode)

import Json.Decode
import Json.Encode
import MidiEvent
import Music
import Song exposing (Song)


encode : Song -> Json.Encode.Value
encode song =
    Json.Encode.list noteToJson (Song.notes song)


noteToJson : Music.NoteEvent -> Json.Encode.Value
noteToJson noteEvent =
    let
        midiEvent : MidiEvent.MidiEvent
        midiEvent =
            MidiEvent.fromNoteEvent noteEvent
    in
    Json.Encode.object
        [ ( "midi", Json.Encode.int midiEvent.pitch )
        , ( "index", Json.Encode.int midiEvent.start )
        ]


decoder : Json.Decode.Decoder Song
decoder =
    notesDecoder
        |> Json.Decode.map Song.new


notesDecoder : Json.Decode.Decoder (List Music.NoteEvent)
notesDecoder =
    Json.Decode.list noteDecoder


noteDecoder : Json.Decode.Decoder Music.NoteEvent
noteDecoder =
    Json.Decode.map2
        (\start pitch ->
            MidiEvent.new
                { start = start
                , pitch = pitch
                }
                |> MidiEvent.toNoteEvent
        )
        (Json.Decode.field "index" Json.Decode.int)
        (Json.Decode.field "midi" Json.Decode.int)
