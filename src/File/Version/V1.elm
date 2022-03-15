module File.Version.V1 exposing (decoder, encode)

import Json.Decode
import Json.Encode
import Music
import Music.Duration
import Music.Note
import Music.Pitch
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
    Json.Encode.object
        [ ( "midiNoteNumber", pitchToJson (Music.Note.pitch note) )
        , ( "duration", durationToJson (Music.Note.duration note) )
        ]


pitchToJson : Music.Pitch.Pitch -> Json.Encode.Value
pitchToJson pitch =
    Json.Encode.int (Music.Pitch.toMIDINoteNumber pitch)


decoder : Json.Decode.Decoder Project.Project
decoder =
    Json.Decode.field "version" Json.Decode.int
        |> Json.Decode.andThen afterVersionDecoder


afterVersionDecoder : Int -> Json.Decode.Decoder Project.Project
afterVersionDecoder version =
    if version /= 1 then
        Json.Decode.fail "failed to decode: unsupported version number"

    else
        Json.Decode.map Project.new
            (Json.Decode.field "noteEvents" noteEventsDecoder)


noteEventsDecoder : Json.Decode.Decoder (List Music.NoteEvent)
noteEventsDecoder =
    Json.Decode.list noteEventDecoder


noteEventDecoder : Json.Decode.Decoder Music.NoteEvent
noteEventDecoder =
    Json.Decode.map2
        (\at value ->
            { at = at
            , value = value
            }
        )
        (Json.Decode.field "at" durationDecoder)
        (Json.Decode.field "value" noteDecoder)


durationDecoder : Json.Decode.Decoder Music.Duration.Duration
durationDecoder =
    Json.Decode.map2
        (\numerator denominator ->
            Music.Duration.fromFraction
                { numerator = numerator
                , denominator = denominator
                }
        )
        (Json.Decode.field "numerator" Json.Decode.int)
        (Json.Decode.field "denominator" Json.Decode.int)


noteDecoder : Json.Decode.Decoder Music.Note.Note
noteDecoder =
    Json.Decode.map2
        (\pitch duration ->
            Music.Note.note pitch duration
        )
        (Json.Decode.field "midiNoteNumber" pitchDecoder)
        (Json.Decode.field "duration" durationDecoder)


pitchDecoder : Json.Decode.Decoder Music.Pitch.Pitch
pitchDecoder =
    Json.Decode.int
        |> Json.Decode.map Music.Pitch.fromMIDINoteNumber
