module TetrixBoard () where

import TetrixPiece
import Graphics.Gloss

boardWidth :: Float
boardWidth = 10

boardHeight :: Float
boardHeight = 22

windowHeight :: Float
windowHeight = 600

windowWidth :: Float
windowWidth = 400

colorTable = [
        -- black
        [0, 0, 0, 255],
        -- red
        [204, 102, 102, 255],
        -- green
        [102, 204, 102, 255],
        -- blue
        [102, 102, 204, 255],
        -- yellow
        [204, 204, 102, 255],
        -- pink
        [204, 102, 204, 255],
        -- cyan
        [102, 204, 204, 255],
        -- beige
        [218, 170, 0, 255]
    ]

data TetrixBoard = TetrixBoard {
    -- TODO this is actually a Qtimer, to handle timer Events, see what fits better later
    _timer :: Int,
    _nextPieceLabel :: Maybe String,
    _isWaitingAfterLine :: Bool,
    _curPiece :: TetrixPiece,
    _nextPiece :: TetrixPiece,
    _curX :: Int,
    _curY :: Int,
    _numLinesRemoved :: Int,
    _numPiecesDropped :: Int,
    _score :: Int,
    _level :: Int,
    _board :: [Shape],
    -- equivalent to setFrameStyle
    _frameStyle :: Picture,
    -- TODO see whats equivalent to focusPolicy, maybe related do EventHandling
    _isStarted :: Bool,
    _isPaused :: Bool
}

createBoard gen = TetrixBoard {
    _timer = 0, 
    _nextPieceLabel = Nothing, 
    _isWaitingAfterLine = False, 
    _curPiece = createPiece, 
    _nextPiece = piece, 
    _curX = 0, 
    _curY = 0, 
    _numLinesRemoved = 0, 
    _numPiecesDropped = 0, 
    _score = 0, 
    _level = 0, 
    _board = [NoShape | _ <- [0..(boardWidth * boardHeight)]], 
    _frameStyle = rectangleSolid boardWidth boardHeight, 
    _isStarted = False, 
    _isPaused = False
    }
    where
    (piece, _) = setRandomShape createPiece gen

shapeAt :: TetrixBoard -> Int -> Int -> Shape
shapeAt board xCoord yCoord = _board board !! (yCoord * round boardWidth + xCoord)

setShapeAt :: TetrixBoard -> Int -> Int -> Shape -> TetrixBoard
setShapeAt board xCoord yCoord shape = board { _board = [if index == yCoord * round boardWidth + xCoord 
                                                         then shape 
                                                         else oldShape | (index, oldShape) <- zip [0..] (_board board) 
                                                        ]}

timeoutTime :: TetrixBoard -> Float
timeoutTime board = 1000 / (1 + fromIntegral (_level board))

squareWidth :: Float
squareWidth  = windowWidth / boardWidth

squareHeight :: Float
squareHeight = windowHeight / boardHeight

setNextPieceLabel :: TetrixBoard -> String -> TetrixBoard
setNextPieceLabel board label = board { _nextPieceLabel = Just label}

clearBoard :: TetrixBoard -> TetrixBoard
clearBoard board = board { _board = [NoShape | _ <- [0..(boardHeight * boardWidth)]]}

drawSquare :: TetrixBoard -> Int -> Int -> Shape -> Picture
drawSquare board xCoord yCoord shape = pictures [topLine, rightLine, bottomLine, leftLine, centerSquare]
                                 where
                                 centerSquare = color (makeColorI r g b a) $
                                                translate centerX centerY $ rectangleSolid (squareWidth - 2) (squareHeight - 2)
                                 leftLine = color (makeColorI (r + 20) (g + 20) (b + 20) a) $
                                            line [
                                                 (fromIntegral xCoord, fromIntegral yCoord + squareHeight - 1), 
                                                 (fromIntegral xCoord, fromIntegral yCoord)
                                                 ]  
                                 bottomLine = color (makeColorI (r + 20) (g + 20) (b + 20) a) $
                                            line [
                                                 (fromIntegral xCoord, fromIntegral yCoord),
                                                 ((fromIntegral xCoord + squareWidth - 1), fromIntegral yCoord) 
                                                 ]  
                                 topLine = color (makeColorI (r - 20) (g - 20) (b - 20) a) $
                                            line [
                                                 (fromIntegral xCoord + 1, fromIntegral yCoord + squareHeight - 1),
                                                 (fromIntegral xCoord + squareWidth - 1, fromIntegral yCoord + squareHeight - 1) 
                                                 ]  
                                 rightLine = color (makeColorI (r - 20) (g - 20) (b - 20) a) $
                                            line [
                                                 (fromIntegral xCoord + squareWidth - 1, fromIntegral yCoord + squareHeight - 1),
                                                 (fromIntegral xCoord + squareWidth - 1, fromIntegral yCoord + 1)
                                                 ]  
                                 r = (colorTable !! (fromEnum shape)) !! 0
                                 g = (colorTable !! (fromEnum shape)) !! 1
                                 b = (colorTable !! (fromEnum shape)) !! 2
                                 a = (colorTable !! (fromEnum shape)) !! 3
                                 centerX = fromIntegral xCoord + squareWidth / 2
                                 centerY = fromIntegral yCoord + squareHeight / 2

