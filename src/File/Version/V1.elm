module File.Version.V1 exposing (..)

import Json.Decode
import Json.Encode
import MidiEvent
import Music
import Music.Duration
import Music.Note
import Project


encode : Project.Project -> Json.Encode.Value
encode project =
    let
        noteEvents : List Music.NoteEvent
        noteEvents =
            Project.noteEvents project
    in
    Json.Encode.object
        [ ( "version", Json.Encode.int 1 )
        , ( "noteEvents", Json.Encode.list noteEventToJson noteEvents )
        ]


noteEventToJson : Music.NoteEvent -> Json.Encode.Value
noteEventToJson noteEvent =
    Json.Encode.object
        [ ( "at", durationToJson noteEvent.at )
        , ( "value", noteToJson noteEvent.value )
        ]


durationToJson : Music.Duration.Duration -> Json.Encode.Value
durationToJson duration =
    case Music.Duration.toFraction duration of
        { numerator, denominator } ->
            Json.Encode.object
                [ ( "numerator", Json.Encode.int numerator )
                , ( "denominator", Json.Encode.int denominator )
                ]


noteToJson : Music.Note.Note -> Json.Encode.Value
noteToJson note =
    []


decoder : Json.Decode.Decoder Project.Project
decoder =
    Json.Decode.field "version" Json.Decode.int
        |> Json.Decode.andThen afterVersionDecoder


afterVersionDecoder : Int -> Json.Decode.Decoder Project.Project
afterVersionDecoder version =
    if version /= 1 then
        Json.Decode.fail "failed to decode: incorrect version number"

    else
        Json.Decode.map Project.new
            (Json.Decode.field "noteEvents" noteEventsDecoder)


noteEventsDecoder : Json.Decode.Decoder (List Music.NoteEvent)
noteEventsDecoder =
    Json.Decode.list noteEventDecoder


noteEventDecoder : Json.Decode.Decoder Music.NoteEvent
noteEventDecoder =
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
