module Coordinate exposing
    ( GridCells, gridCells
    , Pixels, pixels
    , toPixelsRecord
    , roundToNearestScreenCell
    , fromPixelsToGridCells, fromGridCellsToPixels
    )

{-|

@docs GridCells, gridCells
@docs Pixels, pixels

@docs toPixelsRecord

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


toPixelsRecord : Coordinate InPixels -> { x : Int, y : Int }
toPixelsRecord (Coordinate ( x, y )) =
    { x = x
    , y = y
    }


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
