module File.Version.V1 exposing (decoder, encode)

import Json.Decode
import Json.Encode
import Music.Duration
import Music.Event as Event
import Music.Note as Note
import Music.Pitch
import Project


tags :
    { version : String
    , noteEvents : String
    , at : String
    , value : String
    , numerator : String
    , denominator : String
    , midiNoteNumber : String
    , duration : String
    }
tags =
    { version = "version"
    , noteEvents = "noteEvents"
    , at = "at"
    , value = "value"
    , numerator = "numerator"
    , denominator = "denominator"
    , midiNoteNumber = "midiNoteNumber"
    , duration = "duration"
    }


encode : Project.Project -> Json.Encode.Value
encode project =
    let
        noteEvents : List (Event.Event Note.Note)
        noteEvents =
            Project.noteEvents project
    in
    Json.Encode.object
        [ ( tags.version, Json.Encode.int 1 )
        , ( tags.noteEvents, Json.Encode.list noteEventToJson noteEvents )
        ]


noteEventToJson : Event.Event Note.Note -> Json.Encode.Value
noteEventToJson noteEvent =
    Json.Encode.object
        [ ( tags.at, durationToJson noteEvent.at )
        , ( tags.value, noteToJson noteEvent.value )
        ]


durationToJson : Music.Duration.Duration -> Json.Encode.Value
durationToJson duration =
    case Music.Duration.toSerial duration of
        { numerator, denominator } ->
            Json.Encode.object
                [ ( tags.numerator, Json.Encode.int numerator )
                , ( tags.denominator, Json.Encode.int denominator )
                ]


noteToJson : Note.Note -> Json.Encode.Value
noteToJson note =
    Json.Encode.object
        [ ( tags.midiNoteNumber, pitchToJson (Note.pitch note) )
        , ( tags.duration, durationToJson (Note.duration note) )
        ]


pitchToJson : Music.Pitch.Pitch -> Json.Encode.Value
pitchToJson pitch =
    Json.Encode.int (Music.Pitch.toMIDINoteNumber pitch)


decoder : Json.Decode.Decoder Project.Project
decoder =
    Json.Decode.field tags.version Json.Decode.int
        |> Json.Decode.andThen afterVersionDecoder


afterVersionDecoder : Int -> Json.Decode.Decoder Project.Project
afterVersionDecoder version =
    if version /= 1 then
        Json.Decode.fail "failed to decode: unsupported version number"

    else
        Json.Decode.map Project.new
            (Json.Decode.field tags.noteEvents noteEventsDecoder)


noteEventsDecoder : Json.Decode.Decoder (List (Event.Event Note.Note))
noteEventsDecoder =
    Json.Decode.list noteEventDecoder


noteEventDecoder : Json.Decode.Decoder (Event.Event Note.Note)
noteEventDecoder =
    Json.Decode.map2
        (\at value ->
            { at = at
            , value = value
            }
        )
        (Json.Decode.field tags.at durationDecoder)
        (Json.Decode.field tags.value noteDecoder)


durationDecoder : Json.Decode.Decoder Music.Duration.Duration
durationDecoder =
    Json.Decode.map2
        (\numerator denominator ->
            Music.Duration.fromSerial
                { numerator = numerator
                , denominator = denominator
                }
        )
        (Json.Decode.field tags.numerator Json.Decode.int)
        (Json.Decode.field tags.denominator Json.Decode.int)


noteDecoder : Json.Decode.Decoder Note.Note
noteDecoder =
    Json.Decode.map2
        (\pitch duration ->
            Note.note pitch duration
        )
        (Json.Decode.field tags.midiNoteNumber pitchDecoder)
        (Json.Decode.field tags.duration durationDecoder)


pitchDecoder : Json.Decode.Decoder Music.Pitch.Pitch
pitchDecoder =
    Json.Decode.int
        |> Json.Decode.map Music.Pitch.fromMIDINoteNumber
