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


pixels : Int -> Int -> Pixels
pixels =
    new


pianoRoll : Int -> Int -> PianoRoll
pianoRoll =
    new


cellSize : Int
cellSize =
    21


roundToNearestScreenCell : Pixels -> Pixels
roundToNearestScreenCell (Coordinate ( x, y )) =
    Coordinate
        ( x // cellSize * cellSize
        , y // cellSize * cellSize
        )


fromPixelsToPianoRoll : Pixels -> PianoRoll
fromPixelsToPianoRoll (Coordinate ( x, y )) =
    Coordinate
        ( x // cellSize
        , y // cellSize
        )


fromPianoRollToPixels : PianoRoll -> Pixels
fromPianoRollToPixels (Coordinate ( x, y )) =
    Coordinate
        ( x * cellSize
        , y * cellSize
        )
