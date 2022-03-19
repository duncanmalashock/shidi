module PianoRoll exposing
    ( Model, init
    , Msg, update, OutMsg(..)
    , view
    , ClickAction(..)
    )

{-|

@docs Model, init

@docs Msg, update, OutMsg

@docs view

-}

import Html exposing (Html)
import Html.Attributes
import Html.Events
import Json.Decode
import Music
import Music.Duration
import Music.Note
import Music.Pitch
import PianoRoll.Key
import PianoRoll.Piano
import Project


type ScaleX
    = ScaleXSmall
    | ScaleXMedium
    | ScaleXLarge
    | ScaleXGiant


cellSizeX : ScaleX -> Int
cellSizeX scaleX =
    case scaleX of
        ScaleXSmall ->
            14

        ScaleXMedium ->
            21

        ScaleXLarge ->
            36

        ScaleXGiant ->
            50


type ScaleY
    = ScaleYSmall
    | ScaleYMedium
    | ScaleYLarge


cellSizeY : ScaleY -> Int
cellSizeY scaleY =
    case scaleY of
        ScaleYSmall ->
            14

        ScaleYMedium ->
            21

        ScaleYLarge ->
            28


type Model
    = Model
        { mousePosition : Maybe MusicCoordinate
        , scaleX : ScaleX
        , scaleY : ScaleY
        }


init : Model
init =
    Model
        { mousePosition = Nothing
        , scaleX = ScaleXMedium
        , scaleY = ScaleYMedium
        }


type Msg
    = UserMovedMouseOverGrid MusicCoordinate
    | UserMovedMouseAway
    | UserClickedLeftMouseButton MusicCoordinate
    | UserClickedRightMouseButton MusicCoordinate


type OutMsg
    = AddNote Music.PitchEvent
    | RemoveNote Music.PitchEvent


type ClickAction
    = ShouldAddNote
    | ShouldRemoveNote


update :
    { onClick : ClickAction
    }
    -> Msg
    -> Model
    -> ( Model, Maybe OutMsg )
update { onClick } msg (Model model) =
    case msg of
        UserMovedMouseOverGrid newPosition ->
            ( Model
                { model
                    | mousePosition = Just newPosition
                }
            , Nothing
            )

        UserMovedMouseAway ->
            ( Model
                { model
                    | mousePosition = Nothing
                }
            , Nothing
            )

        UserClickedLeftMouseButton coordinate ->
            case onClick of
                ShouldAddNote ->
                    let
                        pitchEvent : Music.PitchEvent
                        pitchEvent =
                            fromMusicToPitchEvent coordinate
                                |> Debug.log "added note"
                    in
                    ( Model model
                    , Just
                        (AddNote pitchEvent)
                    )

                ShouldRemoveNote ->
                    let
                        pitchEvent : Music.PitchEvent
                        pitchEvent =
                            fromMusicToPitchEvent coordinate
                    in
                    ( Model model
                    , Just (RemoveNote pitchEvent)
                    )

        UserClickedRightMouseButton coordinate ->
            let
                pitchEvent : Music.PitchEvent
                pitchEvent =
                    fromMusicToPitchEvent coordinate
            in
            ( Model model
            , Just (RemoveNote pitchEvent)
            )


view :
    { project : Project.Project
    , model : Model
    , toMsg : Msg -> msg
    , onPianoKeyClick : Int -> msg
    , newNoteValue : Music.Duration.Duration
    }
    -> Html msg
view options =
    let
        (Model model) =
            options.model
    in
    Html.div [ Html.Attributes.class "row" ]
        [ PianoRoll.Piano.view
            (cellSizeY model.scaleY)
            options.onPianoKeyClick
        , viewRollWrapper
            { project = options.project
            , model = options.model
            , toMsg = options.toMsg
            , newNoteValue = options.newNoteValue
            }
        ]


viewRollWrapper :
    { project : Project.Project
    , model : Model
    , toMsg : Msg -> msg
    , newNoteValue : Music.Duration.Duration
    }
    -> Html msg
viewRollWrapper options =
    let
        (Model model) =
            options.model
    in
    Html.div
        [ Html.Attributes.class "piano-roll__wrapper" ]
        [ viewRoll options
        , viewNotes
            { project = options.project
            , model = options.model
            }
        , case model.mousePosition of
            Just coordinate ->
                viewNote
                    options.model
                    "mediumseagreen"
                    { at = coordinate.at
                    , value = Music.Note.note coordinate.pitch options.newNoteValue
                    }

            Nothing ->
                Html.text ""
        ]


viewRoll :
    { project : Project.Project
    , model : Model
    , newNoteValue : Music.Duration.Duration
    , toMsg : Msg -> msg
    }
    -> Html msg
viewRoll options =
    let
        (Model model) =
            options.model
    in
    Html.div
        ([ Html.Attributes.class "piano-roll"
         , Html.Attributes.style "background-image"
            (backgroundImageAttr
                { width = cellSizeX model.scaleX
                , height = cellSizeY model.scaleY
                }
            )
         ]
            ++ mouseEvents model.scaleX model.scaleY
        )
        []
        |> Html.map options.toMsg


viewNotes : { project : Project.Project, model : Model } -> Html msg
viewNotes { project, model } =
    Project.noteEvents project
        |> List.map (viewNote model "deeppink")
        |> Html.div []


viewNote : Model -> String -> Music.NoteEvent -> Html msg
viewNote (Model model) color noteEvent =
    let
        { x, y } =
            MusicCoordinate noteEvent.at (Music.Note.pitch noteEvent.value)
                |> fromMusicToPixels model.scaleX model.scaleY

        noteDuration : Music.Duration.Duration
        noteDuration =
            Music.Note.duration noteEvent.value

        width : Int
        width =
            Music.Duration.multiplyByInt
                (cellSizeX model.scaleX * 8)
                noteDuration
                |> Music.Duration.toFloat
                |> Basics.round

        height : Int
        height =
            cellSizeY model.scaleY
    in
    Html.div
        [ Html.Attributes.class "note-preview"
        , Html.Attributes.style "background" color
        , Html.Attributes.style "width" (String.fromInt width ++ "px")
        , Html.Attributes.style "height" (String.fromInt height ++ "px")
        , Html.Attributes.style "transform"
            ("translate($x, $y)"
                |> String.replace "$x" (String.fromInt x ++ "px")
                |> String.replace "$y" (String.fromInt y ++ "px")
            )
        ]
        []


mouseEvents : ScaleX -> ScaleY -> List (Html.Attribute Msg)
mouseEvents scaleX scaleY =
    let
        onMouseMove : Html.Attribute Msg
        onMouseMove =
            Html.Events.on "mousemove"
                (offsetDecoder
                    |> Json.Decode.map (fromPixelsToMusic scaleX scaleY)
                    |> Json.Decode.map UserMovedMouseOverGrid
                )

        onMouseUp : Html.Attribute Msg
        onMouseUp =
            Html.Events.on "mouseup"
                (Json.Decode.andThen
                    (\mouseButton ->
                        if mouseButton == 0 then
                            offsetDecoder
                                |> Json.Decode.map (fromPixelsToMusic scaleX scaleY)
                                |> Json.Decode.map UserClickedLeftMouseButton

                        else if mouseButton == 2 then
                            offsetDecoder
                                |> Json.Decode.map (fromPixelsToMusic scaleX scaleY)
                                |> Json.Decode.map UserClickedRightMouseButton

                        else
                            Json.Decode.fail "bad witch club 🧙\u{200D}♀️"
                    )
                    (Json.Decode.field "button" Json.Decode.int)
                )

        offsetDecoder : Json.Decode.Decoder PixelCoordinate
        offsetDecoder =
            Json.Decode.map2
                PixelCoordinate
                (Json.Decode.field "offsetX" Json.Decode.int)
                (Json.Decode.field "offsetY" Json.Decode.int)
    in
    [ onMouseMove
    , onMouseUp
    , Html.Events.onMouseLeave UserMovedMouseAway
    ]


backgroundImageAttr : { height : Int, width : Int } -> String
backgroundImageAttr options =
    gridBackground
        |> String.replace "$keys" (PianoRoll.Key.view options.height)
        |> String.replace "$verticalLines"
            (List.range 0 4
                |> List.map (viewVerticalLine options)
                |> String.join ""
            )
        |> String.replace "$totalHeight" (String.fromInt (12 * options.height))
        |> String.replace "$midSplit" (String.fromInt (7 * options.height))
        |> String.replace "$verticalLineColor" "#fff5"
        |> String.replace "$horizontalLineColor" "#fff2"
        |> String.replace "$width" (String.fromInt options.width)
        |> String.replace "\n" ""
        |> String.replace "#" "%23"
        |> wrapInUrl


wrapInUrl : String -> String
wrapInUrl input =
    "url('data:image/svg+xml," ++ input ++ "')"


viewVerticalLine : { height : Int, width : Int } -> Int -> String
viewVerticalLine { width, height } index =
    """<line x1="$x" y1="0" x2="$x" y2="$totalHeight" stroke="$verticalLineColor" />"""
        |> String.replace "$x" (String.fromInt (width * index))
        |> String.replace "$totalHeight" (String.fromInt (12 * height))


gridBackground : String
gridBackground =
    """
<svg class="piano-roll__bg" xmlns="http://www.w3.org/2000/svg" viewBox="0 0 $width $totalHeight" width="$width">
  <g class="piano-roll__keys">
    $keys
  </g>
  <g class="piano-roll__lines">
    $verticalLines
    <line x1="0" y1="0" x2="$width" y2="0" stroke="$horizontalLineColor" />
    <line x1="0" y1="$midSplit" x2="$width" y2="$midSplit" stroke="$horizontalLineColor" />
    <line x1="0" y1="$totalHeight" x2="$width" y2="$totalHeight" stroke="$horizontalLineColor" />
  </g>
</svg>
    """



-- Coordinate conversions


type alias PixelCoordinate =
    { x : Int, y : Int }


type alias MusicCoordinate =
    { at : Music.Duration.Duration
    , pitch : Music.Pitch.Pitch
    }


pixelsXToStart : ScaleX -> Int -> Music.Duration.Duration
pixelsXToStart scaleX x =
    Music.Duration.multiplyByInt (x // cellSizeX scaleX) Music.Duration.eighth


startToPixelsX : ScaleX -> Music.Duration.Duration -> Int
startToPixelsX scaleX duration =
    Basics.round (Music.Duration.toFloat duration * 8) * cellSizeX scaleX


pixelsYToPitch : ScaleY -> Int -> Music.Pitch.Pitch
pixelsYToPitch scaleY y =
    (131 - (y // cellSizeY scaleY))
        |> Music.Pitch.fromMIDINoteNumber


pitchToPixelsY : ScaleY -> Music.Pitch.Pitch -> Int
pitchToPixelsY scaleY pitch =
    (131 - Music.Pitch.toMIDINoteNumber pitch)
        * cellSizeY scaleY


fromPixelsToMusic : ScaleX -> ScaleY -> PixelCoordinate -> MusicCoordinate
fromPixelsToMusic scaleX scaleY { x, y } =
    MusicCoordinate (pixelsXToStart scaleX x) (pixelsYToPitch scaleY y)


fromMusicToPixels : ScaleX -> ScaleY -> MusicCoordinate -> PixelCoordinate
fromMusicToPixels scaleX scaleY { at, pitch } =
    { x = startToPixelsX scaleX at
    , y = pitchToPixelsY scaleY pitch
    }


fromMusicToPitchEvent : MusicCoordinate -> Music.PitchEvent
fromMusicToPitchEvent { at, pitch } =
    { at = at
    , value = pitch
    }
