module Zoom exposing
    ( Zoom
    , cellSizeX
    , cellSizeY
    , new
    , viewX
    , viewY
    )

import Html
import Html.Attributes exposing (value)
import Html.Events


type Zoom
    = Zoom { x : Int, y : Int }


new : Zoom
new =
    Zoom { x = 30, y = 21 }


cellSizeX : Zoom -> Int
cellSizeX (Zoom { x }) =
    x


cellSizeY : Zoom -> Int
cellSizeY (Zoom { y }) =
    y


type Axis
    = X
    | Y


viewX : (Zoom -> msg) -> Zoom -> Html.Html msg
viewX toMsg zoom =
    viewHelp X toMsg zoom


viewY : (Zoom -> msg) -> Zoom -> Html.Html msg
viewY toMsg zoom =
    viewHelp Y toMsg zoom


viewHelp : Axis -> (Zoom -> msg) -> Zoom -> Html.Html msg
viewHelp axis toMsg ((Zoom { x, y }) as zoom) =
    let
        { value, updateMsg, min, max, step } =
            case axis of
                X ->
                    { value = x
                    , updateMsg = UserChangedZoomXInput
                    , min = 10
                    , max = 120
                    , step = 5
                    }

                Y ->
                    { value = y
                    , updateMsg = UserChangedZoomYInput
                    , min = 14
                    , max = 56
                    , step = 7
                    }
    in
    Html.input
        [ Html.Attributes.type_ "range"
        , Html.Attributes.min (String.fromInt min)
        , Html.Attributes.max (String.fromInt max)
        , Html.Attributes.step (String.fromInt step)
        , Html.Attributes.value <| String.fromInt value
        , Html.Events.onInput updateMsg
        ]
        []
        |> Html.map (\msg -> update msg zoom |> toMsg)


update : Msg -> Zoom -> Zoom
update msg (Zoom details) =
    case msg of
        UserChangedZoomXInput newStringVal ->
            case String.toInt newStringVal of
                Just newIntVal ->
                    Zoom { details | x = newIntVal }

                Nothing ->
                    Zoom details

        UserChangedZoomYInput newStringVal ->
            case String.toInt newStringVal of
                Just newIntVal ->
                    Zoom { details | y = newIntVal }

                Nothing ->
                    Zoom details


type Msg
    = UserChangedZoomXInput String
    | UserChangedZoomYInput String
