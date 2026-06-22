{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Coord (
    createPiece,
    coord,
    getXCoord,
    getYCoord,
    setXCoord,
    setYCoord,
    getMinX,
    getMinY,
    getMaxX,
    getMaxY
) where

type Block = (Int, Int)
type PieceCoord = (Block, Block, Block, Block)
-- data PieceCoord = PieceCoord Block Block Block Block

-- coordsTable :: (PieceCoord, PieceCoord, PieceCoord, PieceCoord, PieceCoord, PieceCoord, PieceCoord, PieceCoord)
-- coordsTable = (
--         -- NoShape
--         ((0, 0), (0, 0), (0, 0), (0, 0)),
--         -- ZShape
--         ((0, -1), (0, 0), (-1, 0), (-1, 1)),
--         -- SShape
--         ((0, -1), (0, 0), (1, 0), (1, 1)),
--         -- LineShape
--         ((0, -1), (0, 0), (0, 1), (0, 2)),
--         -- TShape
--         ((-1, 0), (0, 0), (1, 0), (0, 1)),
--         -- SquareShape
--         ((0, 0), (1, 0), (0, 1), (1, 1)),
--         -- LShape
--         ((-1, -1), (0, -1), (0, 0), (0, 1)),
--         -- MirroredLShape
--         ((1, -1), (0, -1), (0, 0), (0, 1))
--     )

data TetrixPiece = NoShape | ZShape | SShape | LineShape | TShape | SquareShape | LShape | MirroredLShape deriving Eq

class Coord a where
    coord :: a -> PieceCoord

instance Coord TetrixPiece where
    coord NoShape        = (( 0,  0), (0,  0), ( 0, 0), ( 0, 0))
    coord ZShape         = (( 0, -1), (0,  0), (-1, 0), (-1, 1))
    coord SShape         = (( 0, -1), (0,  0), ( 1, 0), ( 1, 1))
    coord LineShape      = (( 0, -1), (0,  0), ( 0, 1), ( 0, 2))
    coord TShape         = ((-1,  0), (0,  0), ( 1, 0), ( 0, 1))
    coord SquareShape    = (( 0,  0), (1,  0), ( 0, 1), ( 1, 1))
    coord LShape         = ((-1, -1), (0, -1), ( 0, 0), ( 0, 1))
    coord MirroredLShape = (( 1, -1), (0, -1), ( 0, 0), ( 0, 1))

-- instance Coord TetrixPiece where
--     coord NoShape        = PieceCoord ( 0,  0) (0,  0) ( 0, 0) ( 0, 0)
--     coord ZShape         = PieceCoord ( 0, -1) (0,  0) (-1, 0) (-1, 1)
--     coord SShape         = PieceCoord ( 0, -1) (0,  0) ( 1, 0) ( 1, 1)
--     coord LineShape      = PieceCoord ( 0, -1) (0,  0) ( 0, 1) ( 0, 2)
--     coord TShape         = PieceCoord (-1,  0) (0,  0) ( 1, 0) ( 0, 1)
--     coord SquareShape    = PieceCoord ( 0,  0) (1,  0) ( 0, 1) ( 1, 1)
--     coord LShape         = PieceCoord (-1, -1) (0, -1) ( 0, 0) ( 0, 1)
--     coord MirroredLShape = PieceCoord ( 1, -1) (0, -1) ( 0, 0) ( 0, 1)

createPiece :: TetrixPiece
createPiece = NoShape

getXCoord :: PieceCoord -> Int -> Maybe Int
getXCoord (b,_,_,_) 0 = Just (fst b) 
getXCoord (_,b,_,_) 1 = Just (fst b) 
getXCoord (_,_,b,_) 2 = Just (fst b)
getXCoord (_,_,_,b) 3 = Just (fst b)
getXCoord _ _         = Nothing

getYCoord :: PieceCoord -> Int -> Maybe Int
getYCoord (b,_,_,_) 0 = Just (snd b) 
getYCoord (_,b,_,_) 1 = Just (snd b) 
getYCoord (_,_,b,_) 2 = Just (snd b)
getYCoord (_,_,_,b) 3 = Just (snd b)
getYCoord _ _         = Nothing

setXCoord :: PieceCoord -> Int -> Int -> Maybe PieceCoord
setXCoord ((_, b0y), (b1x, b1y), (b2x, b2y), (b3x, b3y)) 0 newX = Just ((newX, b0y), (b1x, b1y), (b2x, b2y), (b3x, b3y))
setXCoord ((b0x, b0y), (_, b1y), (b2x, b2y), (b3x, b3y)) 1 newX = Just ((b0x, b0y), (newX, b1y), (b2x, b2y), (b3x, b3y))
setXCoord ((b0x, b0y), (b1x, b1y), (_, b2y), (b3x, b3y)) 2 newX = Just ((b0x, b0y), (b1x, b1y), (newX, b2y), (b3x, b3y))
setXCoord ((b0x, b0y), (b1x, b1y), (b2x, b2y), (_, b3y)) 3 newX = Just ((b0x, b0y), (b1x, b1y), (b2x, b2y), (newX, b3y))
setXCoord _ _ _                                                 = Nothing

setYCoord :: PieceCoord -> Int -> Int -> Maybe PieceCoord
setYCoord ((b0x, _), (b1x, b1y), (b2x, b2y), (b3x, b3y)) 0 newY = Just ((b0x, newY), (b1x, b1y), (b2x, b2y), (b3x, b3y))
setYCoord ((b0x, b0y), (b1x, _), (b2x, b2y), (b3x, b3y)) 1 newY = Just ((b0x, b0y), (b1x, newY), (b2x, b2y), (b3x, b3y))
setYCoord ((b0x, b0y), (b1x, b1y), (b2x, _), (b3x, b3y)) 2 newY = Just ((b0x, b0y), (b1x, b1y), (b2x, newY), (b3x, b3y))
setYCoord ((b0x, b0y), (b1x, b1y), (b2x, b2y), (b3x, _)) 3 newY = Just ((b0x, b0y), (b1x, b1y), (b2x, b2y), (b3x, newY))
setYCoord _ _ _                                                 = Nothing


blockTupleToListTuple :: (Block, Block, Block, Block) -> [Block]
blockTupleToListTuple (b1, b2, b3, b4) = [b1, b2, b3, b4]

getMinX :: PieceCoord -> Int
getMinX pieceCoord = 
    foldr (\x minX -> if x < minX then x else minX) firstX xCoordList
    where
        xCoordList = map (\(x, _) -> x) (blockTupleToListTuple pieceCoord)
        firstX = xCoordList !! 0

getMinY :: PieceCoord -> Int
getMinY pieceCoord = 
    foldr (\y minY -> if y < minY then y else minY) firstY yCoordList
    where
        yCoordList = map (\(_, y) -> y) (blockTupleToListTuple pieceCoord)
        firstY = yCoordList !! 0

getMaxX :: PieceCoord -> Int
getMaxX pieceCoord = 
    foldr (\x maxX -> if x > maxX then x else maxX) firstX xCoordList
    where
        xCoordList = map (\(x, _) -> x) (blockTupleToListTuple pieceCoord)
        firstX = xCoordList !! 0

getMaxY :: PieceCoord -> Int
getMaxY pieceCoord = 
    foldr (\y maxY -> if y < maxY then y else maxY) firstY yCoordList
    where
        yCoordList = map (\(_, y) -> y) (blockTupleToListTuple pieceCoord)
        firstY = yCoordList !! 0

rotateRight :: TetrixPiece -> TetrixPiece
rotateRight piece = if piece == SquareShape 
                    then piece
                    else piece 
