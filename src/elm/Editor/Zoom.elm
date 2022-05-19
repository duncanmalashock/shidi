module Editor.Zoom exposing
    ( Zoom
    , cellSizeX
    , cellSizeY
    , new
    )


type Zoom
    = Zoom ZoomX ZoomY


new : Zoom
new =
    Zoom ZoomXMedium ZoomYMedium


type ZoomX
    = ZoomXSmall
    | ZoomXMedium
    | ZoomXLarge
    | ZoomXGiant


cellSizeX : Zoom -> Int
cellSizeX (Zoom x _) =
    case x of
        ZoomXSmall ->
            14

        ZoomXMedium ->
            21

        ZoomXLarge ->
            36

        ZoomXGiant ->
            50


type ZoomY
    = ZoomYSmall
    | ZoomYMedium
    | ZoomYLarge


cellSizeY : Zoom -> Int
cellSizeY (Zoom _ y) =
    case y of
        ZoomYSmall ->
            14

        ZoomYMedium ->
            21

        ZoomYLarge ->
            28
