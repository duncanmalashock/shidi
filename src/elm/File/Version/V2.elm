module File.Version.V2 exposing (decoder, encode)

import Json.Decode
import Json.Encode
import Music.Duration as Duration
import Music.Dynamics as Dynamics
import Music.Event as Event
import Music.Key as Key
import Music.Meter as Meter
import Music.Note as Note
import Music.Pitch as Pitch
import Music.PitchClass as PitchClass
import Music.Tempo as Tempo
import Project


tags :
    { version : String
    , noteEvents : String
    , at : String
    , value : String
    , numerator : String
    , denominator : String
    , pitch : String
    , duration : String
    , dynamics : String
    , pitchClass : String
    , octave : String
    , letter : String
    , offset : String
    , tempo : String
    }
tags =
    { version = "version"
    , noteEvents = "noteEvents"
    , at = "at"
    , value = "value"
    , numerator = "numerator"
    , denominator = "denominator"
    , pitch = "pitch"
    , duration = "duration"
    , dynamics = "dynamics"
    , pitchClass = "pitchClass"
    , octave = "octave"
    , letter = "letter"
    , offset = "offset"
    , tempo = "tempo"
    }


decoder : Json.Decode.Decoder Project.Project
decoder =
    Json.Decode.field tags.version Json.Decode.int
        |> Json.Decode.andThen afterVersionDecoder


afterVersionDecoder : Int -> Json.Decode.Decoder Project.Project
afterVersionDecoder version =
    let
        toNewProject : List (Event.Event Note.Note) -> Int -> Project.Project
        toNewProject noteEvents tempo =
            Project.new
                { noteEvents = noteEvents
                , tempo = Tempo.quarterNotesPerMinute tempo
                , meter = Meter.fourFour
                , key = Key.c
                }
    in
    if version /= 2 then
        Json.Decode.fail "failed to decode: unsupported version number"

    else
        Json.Decode.map2 toNewProject
            (Json.Decode.field tags.noteEvents
                (noteEventsDecoder
                    |> Json.Decode.map
                        (List.map
                            (Event.fromSerial Note.fromSerial)
                            >> List.filterMap identity
                        )
                )
            )
            (Json.Decode.field tags.tempo Json.Decode.int)


noteEventsDecoder : Json.Decode.Decoder (List (Event.Serial Note.Serial))
noteEventsDecoder =
    Json.Decode.list (eventDecoder noteDecoder)


eventDecoder : Json.Decode.Decoder a -> Json.Decode.Decoder (Event.Serial a)
eventDecoder valueDecoder =
    Json.Decode.map2
        (\at value ->
            { at = at
            , value = value
            }
        )
        (Json.Decode.field tags.at durationDecoder)
        (Json.Decode.field tags.value valueDecoder)


noteDecoder : Json.Decode.Decoder Note.Serial
noteDecoder =
    Json.Decode.map3 Note.Serial
        (Json.Decode.field tags.pitch pitchDecoder)
        (Json.Decode.field tags.duration durationDecoder)
        (Json.Decode.field tags.dynamics dynamicsDecoder)


pitchDecoder : Json.Decode.Decoder Pitch.Serial
pitchDecoder =
    Json.Decode.map2 Pitch.Serial
        (Json.Decode.field tags.pitchClass pitchClassDecoder)
        (Json.Decode.field tags.octave Json.Decode.int)


pitchClassDecoder : Json.Decode.Decoder PitchClass.Serial
pitchClassDecoder =
    Json.Decode.map2 PitchClass.Serial
        (Json.Decode.field tags.letter Json.Decode.string)
        (Json.Decode.field tags.offset Json.Decode.int)


durationDecoder : Json.Decode.Decoder Duration.Serial
durationDecoder =
    Json.Decode.map2 Duration.Serial
        (Json.Decode.field tags.numerator Json.Decode.int)
        (Json.Decode.field tags.denominator Json.Decode.int)


dynamicsDecoder : Json.Decode.Decoder Dynamics.Serial
dynamicsDecoder =
    Json.Decode.int


encode : Project.Project -> Json.Encode.Value
encode project =
    let
        noteEvents : List (Event.Event Note.Note)
        noteEvents =
            Project.noteEvents project

        tempo : Int
        tempo =
            Project.tempo project
                |> Tempo.toSerial
                |> .beatsPerMinute
    in
    Json.Encode.object
        [ ( tags.version, Json.Encode.int 2 )
        , ( tags.noteEvents
          , Json.Encode.list
                (Event.toSerial Note.toSerial >> noteEventToJson)
                noteEvents
          )
        , ( tags.tempo, Json.Encode.int tempo )
        ]


noteEventToJson : Event.Serial Note.Serial -> Json.Encode.Value
noteEventToJson noteEvent =
    Json.Encode.object
        [ ( tags.at, durationToJson noteEvent.at )
        , ( tags.value, noteToJson noteEvent.value )
        ]


noteToJson : Note.Serial -> Json.Encode.Value
noteToJson note =
    Json.Encode.object
        [ ( tags.pitch, pitchToJson note.pitch )
        , ( tags.duration, durationToJson note.duration )
        , ( tags.dynamics, Json.Encode.int note.dynamics )
        ]


pitchToJson : Pitch.Serial -> Json.Encode.Value
pitchToJson pitch =
    Json.Encode.object
        [ ( tags.pitchClass, pitchClassToJson pitch.pitchClass )
        , ( tags.octave, Json.Encode.int pitch.octave )
        ]


pitchClassToJson : PitchClass.Serial -> Json.Encode.Value
pitchClassToJson pitchClass =
    Json.Encode.object
        [ ( tags.letter, Json.Encode.string pitchClass.letter )
        , ( tags.offset, Json.Encode.int pitchClass.offset )
        ]


durationToJson : Duration.Serial -> Json.Encode.Value
durationToJson duration =
    case duration of
        { numerator, denominator } ->
            Json.Encode.object
                [ ( tags.numerator, Json.Encode.int numerator )
                , ( tags.denominator, Json.Encode.int denominator )
                ]
