module Coordinate exposing
    ( PianoRoll, pianoRoll
    , Pixels, pixels
    , toPixelsRecord, toPianoRollRecord
    , roundToNearestScreenCell
    , fromPixelsToPianoRoll, fromPianoRollToPixels
    )

{-|

@docs PianoRoll, pianoRoll
@docs Pixels, pixels

@docs toPixelsRecord, toPianoRollRecord

@docs roundToNearestScreenCell

@docs fromPixelsToPianoRoll, fromPianoRollToPixels

-}


type Coordinate reference
    = Coordinate ( Int, Int )


type alias Pixels =
    Coordinate InPixels


type alias PianoRoll =
    Coordinate InPianoRoll


type InPixels
    = Pixels


type InPianoRoll
    = PianoRoll


new : Int -> Int -> Coordinate any
new x y =
    Coordinate ( x, y )


toRecord : Coordinate any -> { x : Int, y : Int }
toRecord (Coordinate ( x, y )) =
    { x = x
    , y = y
    }


toPixelsRecord : Pixels -> { x : Int, y : Int }
toPixelsRecord =
    toRecord


toPianoRollRecord : PianoRoll -> { x : Int, y : Int }
toPianoRollRecord =
    toRecord


pixels : Int -> Int -> Coordinate InPixels
pixels =
    new


pianoRoll : Int -> Int -> Coordinate InPianoRoll
pianoRoll =
    new


cellSize : Int
cellSize =
    21


roundToNearestScreenCell : Coordinate InPixels -> Coordinate InPixels
roundToNearestScreenCell (Coordinate ( x, y )) =
    Coordinate
        ( x // cellSize * cellSize
        , y // cellSize * cellSize
        )


fromPixelsToPianoRoll : Coordinate InPixels -> Coordinate InPianoRoll
fromPixelsToPianoRoll (Coordinate ( x, y )) =
    Coordinate
        ( x // cellSize
        , y // cellSize
        )


fromPianoRollToPixels : Coordinate InPianoRoll -> Coordinate InPixels
fromPianoRollToPixels (Coordinate ( x, y )) =
    Coordinate
        ( x * cellSize
        , y * cellSize
        )
