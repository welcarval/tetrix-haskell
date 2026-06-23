module TetrixBoard () where

import TetrixPiece
import Graphics.Gloss
import System.Random

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
    _nextPieceLabel :: Maybe Picture,
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
    _isPaused :: Bool,
    _stdGen :: StdGen
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
    _isPaused = False,
    _stdGen = newGen
    }
    where
    (piece, newGen) = setRandomShape createPiece gen

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

setNextPieceLabel :: TetrixBoard -> Picture -> TetrixBoard
setNextPieceLabel board label = board { _nextPieceLabel = Just label}

clearBoard :: TetrixBoard -> TetrixBoard
clearBoard board = board { _board = [NoShape | _ <- [0..(boardHeight * boardWidth)]]}

removeFullLines :: TetrixBoard -> TetrixBoard
removeFullLines board = finalBoard
    where
        finalBoard = 
            if nfl > 0
                then b {
                    _numLinesRemoved = _numLinesRemoved b + nfl,
                    _score = _score b + 10 * nfl,
                    -- TODO timer logic here
                    _isWaitingAfterLine = True,
                    _curPiece = setShape (_curPiece b) NoShape
                }
                else
                    b

        columnList :: [Int]
        columnList = [0 .. round boardWidth]

        reverseRowList :: [Int]
        reverseRowList = reverse [0.. round boardHeight - 1]

        (b, nfl) = processRows board 0 reverseRowList

        processRows :: TetrixBoard -> Int -> [Int] -> (TetrixBoard, Int)
        processRows tb numFullLines []         = (tb, numFullLines)
        processRows tb numFullLines (row:rows) = processRows newBoard newFullLines rows
            where
                isRowFull :: Int -> [Int] -> Bool
                isRowFull _ []               = True
                isRowFull yCoord (xCoord:xs) =
                    if actualShape == NoShape then False else isRowFull yCoord xs 
                        where
                            actualShape = shapeAt board xCoord yCoord

                rowIsFull = isRowFull row columnList

                newFullLines = 
                    if rowIsFull 
                        then numFullLines + 1 
                        else numFullLines

                newBoard = 
                    if rowIsFull 
                        then clearRow (updateRow tb row columnList) row columnList 
                        else tb

        clearRow :: TetrixBoard -> Int -> [Int] -> TetrixBoard
        clearRow board1 _ [] = board1
        clearRow board1 yCoord (xCoord:xs) = clearRow nextBoard yCoord xs
            where
                nextBoard = setShapeAt board1 xCoord yCoord NoShape

        updateRow :: TetrixBoard -> Int -> [Int] -> TetrixBoard
        updateRow board1 _ [] = board1
        updateRow board1 yCoord (xCoord:xs) = updateRow nextBoard yCoord xs 
            where
                nextBoard = setShapeAt board1 xCoord yCoord upperShape 
                    where
                        upperShape = shapeAt board1 xCoord (yCoord + 1)

newPiece :: TetrixBoard -> TetrixBoard
newPiece board = board4
    where
    (nextPiece, newGen) = setRandomShape (_nextPiece board) (_stdGen board)
    board1 = board {
        _curPiece = _nextPiece board,
        _nextPiece = nextPiece,
        _stdGen = newGen
    }
    board2 = showNextPiece board1
    board3 = board2 {
        _curX = truncate (boardWidth / (2 + 1)),
        _curY = round boardHeight - 1 + minY (_curPiece board2)
    }
    board4 = if not (snd (tryMove board3 (_curPiece board3) (_curX board3) (_curY board3)))
             then board3 {
                 _curPiece = setShape (_curPiece board3) NoShape,
                 _timer = 0,
                 _isStarted = False
             }
             else board3

showNextPiece :: TetrixBoard -> TetrixBoard
showNextPiece board = 
    case _nextPieceLabel board of
        Just _ -> board
        Nothing -> board { _nextPieceLabel = Just (Pictures squares)}
    where
        dx = maxX nextPiece - minX nextPiece
        dy = maxY nextPiece - minY nextPiece
        nextPiece = _nextPiece board
        squares = [drawSquare (x nextPiece i) (y nextPiece i) (_shape nextPiece) | i <- [0..3]]

tryMove :: TetrixBoard -> TetrixPiece -> Int -> Int -> (TetrixBoard, Bool)
tryMove board piece newX newY = 
    if all isIndexValid [0..3]
    then (board { _curPiece = piece, _curX = newX, _curY = newY }, True)
    else (board, False)
    where
        isIndexValid :: Int -> Bool
        isIndexValid i = getX i >= 0 && getX i <= round boardWidth && getY i >= 0 && getY i <= round boardHeight
        getX i = newX + x piece i
        getY i = newY + y piece i

drawSquare :: Int -> Int -> Shape -> Picture
drawSquare xCoord yCoord shape = pictures [topLine, rightLine, bottomLine, leftLine, centerSquare]
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

