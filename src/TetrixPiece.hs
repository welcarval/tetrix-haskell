module TetrixPiece (
    createPiece,
    TetrixPiece,
    _shape,
    Shape (NoShape, ZShape, SShape, LineShape, TShape, SquareShape, LShape, MirroredLShape),
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
) where

import System.Random

data Shape = NoShape | ZShape | SShape | LineShape | TShape | SquareShape | LShape | MirroredLShape deriving (Eq, Enum)

data Point = Point {_px :: Int, _py :: Int} deriving (Eq, Show)

data Blocks = Blocks Point Point Point Point deriving (Eq, Show)

data TetrixPiece = TetrixPiece {_blocks :: Blocks, _shape :: Shape}

shapeCoords :: Shape -> Blocks
shapeCoords NoShape        = Blocks (Point 0 0)    (Point 0 0)   (Point 0 0)   (Point 0 0)
shapeCoords ZShape         = Blocks (Point 0 (-1)) (Point 0 0)   (Point (-1) 0)(Point (-1) 1)
shapeCoords SShape         = Blocks (Point 0 (-1)) (Point 0 0)   (Point 1 0)   (Point 1 1)
shapeCoords LineShape      = Blocks (Point 0 (-1)) (Point 0 0)   (Point 0 1)   (Point 0 2)
shapeCoords TShape         = Blocks (Point (-1) 0) (Point 0 0)   (Point 1 0)   (Point 0 1)
shapeCoords SquareShape    = Blocks (Point 0 0)    (Point 1 0)   (Point 0 1)   (Point 1 1)
shapeCoords LShape         = Blocks (Point (-1) (-1)) (Point 0 (-1)) (Point 0 0) (Point 0 1)
shapeCoords MirroredLShape = Blocks (Point 1 (-1)) (Point 0 (-1)) (Point 0 0) (Point 0 1)

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
        _ -> p3
  where
    Blocks p0 p1 p2 p3 = blocks

createPiece :: TetrixPiece
createPiece = TetrixPiece{ _blocks = shapeCoords NoShape, _shape = NoShape}

getShape :: TetrixPiece -> Shape
getShape piece = _shape piece

setShape :: TetrixPiece -> Shape -> TetrixPiece
setShape piece shape = piece{ _blocks = shapeCoords shape, _shape = shape}

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
        else piece { _blocks = newBlocks }
  where
    newBlocks = mapBlocks (\(Point xCoord yCoord) -> Point yCoord (-xCoord)) (_blocks piece)

rotateRight :: TetrixPiece -> TetrixPiece
rotateRight piece =
    if _shape piece == SquareShape
        then piece
        else piece { _blocks = newBlocks }
  where
    newBlocks = mapBlocks (\(Point xCoord yCoord) -> Point (-yCoord) xCoord) (_blocks piece)
