module Editor.Scale exposing
    ( ScaleX(..)
    , ScaleY(..)
    , cellSizeX
    , cellSizeY
    )


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
