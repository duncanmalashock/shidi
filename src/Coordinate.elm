module Coordinate exposing
    ( GridCells, gridCells
    , Pixels, pixels
    , toPixelsRecord, toGridCellsRecord
    , roundToNearestScreenCell
    , fromPixelsToGridCells, fromGridCellsToPixels
    )

{-|

@docs GridCells, gridCells
@docs Pixels, pixels

@docs toPixelsRecord, toGridCellsRecord

@docs roundToNearestScreenCell

@docs fromPixelsToGridCells, fromGridCellsToPixels

-}


type Coordinate reference
    = Coordinate ( Int, Int )


type alias Pixels =
    Coordinate InPixels


type alias GridCells =
    Coordinate InGridCells


type InPixels
    = Pixels


type InGridCells
    = GridCells


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


toGridCellsRecord : GridCells -> { x : Int, y : Int }
toGridCellsRecord =
    toRecord


pixels : Int -> Int -> Coordinate InPixels
pixels =
    new


gridCells : Int -> Int -> Coordinate InGridCells
gridCells =
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


fromPixelsToGridCells : Coordinate InPixels -> Coordinate InGridCells
fromPixelsToGridCells (Coordinate ( x, y )) =
    Coordinate
        ( x // cellSize
        , y // cellSize
        )


fromGridCellsToPixels : Coordinate InGridCells -> Coordinate InPixels
fromGridCellsToPixels (Coordinate ( x, y )) =
    Coordinate
        ( x * cellSize
        , y * cellSize
        )
