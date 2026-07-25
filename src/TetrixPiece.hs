module TetrixPiece (
    createPiece,
    TetrixPiece,
    _shape,
    Shape (NoShape, ZShape, SShape),
    setRandomShape,
    setShape,
    x,
    y,
    maxX,
    maxY,
    minX,
    minY,
    rotateRight,
    rotateLeft,
    coordsTable,
) where

import System.Random

coordsTable :: [[[Int]]]
coordsTable =
    [ -- NoShape
      [[0, 0], [0, 0], [0, 0], [0, 0]]
    , -- ZShape
      [[0, -1], [0, 0], [-1, 0], [-1, 1]]
    , -- SShape
      [[0, -1], [0, 0], [1, 0], [1, 1]]
    , -- LineShape
      [[0, -1], [0, 0], [0, 1], [0, 2]]
    , -- TShape
      [[-1, 0], [0, 0], [1, 0], [0, 1]]
    , -- SquareShape
      [[0, 0], [1, 0], [0, 1], [1, 1]]
    , -- LShape
      [[-1, -1], [0, -1], [0, 0], [0, 1]]
    , -- MirroredLShape
      [[1, -1], [0, -1], [0, 0], [0, 1]]
    ]

data Shape = NoShape | ZShape | SShape | LineShape | TShape | SquareShape | LShape | MirroredLShape deriving (Eq, Enum)

data Point = Point {_px :: Int, _py :: Int} deriving (Eq, Show)

data Blocks = Blocks Point Point Point Point deriving (Eq, Show)

data TetrixPiece = TetrixPiece {_blocks :: Blocks, _shape :: Shape}

pointFromList :: [Int] -> Point
pointFromList [xCoord, yCoord] = Point xCoord yCoord
pointFromList coords = error ("pointFromList: was waiting [x, y], but instead received " ++ show coords)

blocksFromList :: [[Int]] -> Blocks
blocksFromList [a, b, c, d] = Blocks (pointFromList a) (pointFromList b) (pointFromList c) (pointFromList d)
blocksFromList coordsList = error ("blocksFromList: was waiting 4 points, but instead received " ++ show (length coordsList))

blocksToList :: Blocks -> [Point]
blocksToList (Blocks p0 p1 p2 p3) = [p0, p1, p2, p3]

mapBlocks :: (Point -> Point) -> Blocks -> Blocks
mapBlocks f (Blocks p0 p1 p2 p3) = Blocks (f p0) (f p1) (f p2) (f p3)

blockAt :: Blocks -> Int -> Point
blockAt blocks index =
    case index of
        0 -> p0
        1 -> p1
        2 -> p2
        3 -> p3
        _ -> error ("blockAt: invalid block index (was wainting 0..3): " ++ show index)
  where
    Blocks p0 p1 p2 p3 = blocks

createPiece :: TetrixPiece
createPiece = TetrixPiece{_blocks = blocksFromList (coordsTable !! (fromEnum NoShape)), _shape = NoShape}

getShape :: TetrixPiece -> Shape
getShape piece = _shape piece

setShape :: TetrixPiece -> Shape -> TetrixPiece
setShape piece shape = piece{_blocks = blocksFromList (coordsTable !! (fromEnum shape)), _shape = shape}

setRandomShape :: TetrixPiece -> StdGen -> (TetrixPiece, StdGen)
setRandomShape piece gen = (setShape piece (toEnum randomNumber), newGen)
  where
    randomNumber :: Int
    newGen :: StdGen
    (randomNumber, newGen) = randomR (1, 7) gen

x :: TetrixPiece -> Int -> Int
x piece index = _px (blockAt (_blocks piece) index)

y :: TetrixPiece -> Int -> Int
y piece index = _py (blockAt (_blocks piece) index)

-- setX :: TetrixPiece -> Int -> Int -> TetrixPiece
-- setX piece index newX = piece { _coords = [
--                                           if blockIndex == index
--                                           then [newX, (_coords piece !! index) !! 1]
--                                           else blockCoord | (blockIndex, blockCoord) <- zip [0..] (_coords piece)
--                                           ]
--                               }
--
--
--
-- setY :: TetrixPiece -> Int -> Int -> TetrixPiece
-- setY piece index newY = piece { _coords = [
--                                           if blockIndex == index
--                                           then [(_coords piece !! index) !! 0, newY]
--                                           else blockCoord | (blockIndex, blockCoord) <- zip [0..] (_coords piece)
--                                           ]
--                               }

minX :: TetrixPiece -> Int
minX piece = foldr min initialX xList
  where
    initialX = x piece 0
    xList = map _px (blocksToList (_blocks piece))

maxX :: TetrixPiece -> Int
maxX piece = foldr max initialX xList
  where
    initialX = x piece 0
    xList = map _px (blocksToList (_blocks piece))

minY :: TetrixPiece -> Int
minY piece = foldr min initialY yList
  where
    initialY = y piece 0
    yList = map _py (blocksToList (_blocks piece))

maxY :: TetrixPiece -> Int
maxY piece = foldr max initialY yList
  where
    initialY = y piece 0
    yList = map _py (blocksToList (_blocks piece))

rotateLeft :: TetrixPiece -> TetrixPiece
rotateLeft piece =
    if _shape piece == SquareShape
        then piece
        else
            piece
                { _blocks = newBlocks
                }
  where
    newBlocks = mapBlocks (\(Point xCoord yCoord) -> Point yCoord (-xCoord)) (_blocks piece)

rotateRight :: TetrixPiece -> TetrixPiece
rotateRight piece =
    if _shape piece == SquareShape
        then piece
        else
            piece
                { _blocks = newBlocks
                }
  where
    newBlocks = mapBlocks (\(Point xCoord yCoord) -> Point (-yCoord) xCoord) (_blocks piece)
