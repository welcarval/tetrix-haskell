module TetrixPiece (
    createPiece,
    TetrixPiece,
    _shape,
    Shape(NoShape),
    setRandomShape,
    setShape,
    x,
    y,
    maxX,
    maxY,
    minX,
    minY,
    rotateRight,
    rotateLeft
) where

import System.Random

coordsTable :: [[[Int]]]
coordsTable = [
        -- NoShape
        [[0, 0], [0, 0], [0, 0], [0, 0]],
        -- ZShape
        [[0, -1], [0, 0], [-1, 0], [-1, 1]],
        -- SShape
        [[0, -1], [0, 0], [1, 0], [1, 1]],
        -- LineShape
        [[0, -1], [0, 0], [0, 1], [0, 2]],
        -- TShape
        [[-1, 0], [0, 0], [1, 0], [0, 1]],
        -- SquareShape
        [[0, 0], [1, 0], [0, 1], [1, 1]],
        -- LShape
        [[-1, -1], [0, -1], [0, 0], [0, 1]],
        -- MirroredLShape
        [[1, -1], [0, -1], [0, 0], [0, 1]]
    ]

data Shape = NoShape | ZShape | SShape | LineShape | TShape | SquareShape | LShape | MirroredLShape deriving (Eq, Enum)
data TetrixPiece = TetrixPiece { _coords :: [[Int]], _shape :: Shape}

createPiece :: TetrixPiece
createPiece = TetrixPiece {_coords = coordsTable !! (fromEnum NoShape), _shape = NoShape}

getShape :: TetrixPiece -> Shape
getShape piece = _shape piece

setShape :: TetrixPiece -> Shape -> TetrixPiece
setShape piece shape = piece { _coords = coordsTable !! (fromEnum shape), _shape = shape }

setRandomShape :: TetrixPiece -> StdGen -> (TetrixPiece, StdGen)
setRandomShape piece gen = (setShape piece (toEnum randomNumber), newGen)
                           where
                                randomNumber :: Int
                                newGen :: StdGen
                                (randomNumber, newGen) = randomR (1, 7) gen

x :: TetrixPiece -> Int -> Int
x piece index = _coords piece !! index !! 0

y :: TetrixPiece -> Int -> Int
y piece index = _coords piece !! index !! 1

setX :: TetrixPiece -> Int -> Int -> TetrixPiece
setX piece index newX = piece { _coords = [ 
                                          if blockIndex == index 
                                          then [newX, (_coords piece !! index) !! 1] 
                                          else blockCoord | (blockIndex, blockCoord) <- zip [0..] (_coords piece)
                                          ] 
                              }



setY :: TetrixPiece -> Int -> Int -> TetrixPiece
setY piece index newY = piece { _coords = [ 
                                          if blockIndex == index 
                                          then [(_coords piece !! index) !! 0, newY] 
                                          else blockCoord | (blockIndex, blockCoord) <- zip [0..] (_coords piece)
                                          ] 
                              }

minX :: TetrixPiece -> Int
minX piece = foldr min initialX xList
             where
                 initialX = x piece 0
                 xList = map (\pos -> pos !! 0) (_coords piece)


maxX :: TetrixPiece -> Int
maxX piece = foldr max initialX xList
             where
                 initialX = x piece 0
                 xList = map (\pos -> pos !! 0) (_coords piece)

minY :: TetrixPiece -> Int
minY piece = foldr min initialY yList
             where
                 initialY = y piece 0
                 yList = map (\pos -> pos !! 1) (_coords piece)


maxY :: TetrixPiece -> Int
maxY piece = foldr max initialY yList
             where
                 initialY = y piece 0
                 yList = map (\pos -> pos !! 1) (_coords piece)


rotateLeft :: TetrixPiece -> TetrixPiece
rotateLeft piece = if _shape piece == SquareShape 
                    then piece 
                    else piece {
                        _coords = newCoords
                    }
                    where
                        newCoords = [[yCoord, -xCoord] | [xCoord, yCoord] <- _coords piece]


rotateRight :: TetrixPiece -> TetrixPiece
rotateRight piece = if _shape piece == SquareShape 
                    then piece 
                    else piece {
                        _coords = newCoords
                    }
                    where
                        newCoords = [[-yCoord, xCoord] | [xCoord, yCoord] <- _coords piece]
