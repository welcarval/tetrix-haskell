{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module TetrixBoard (
    TetrixBoard,
    createBoard, 
    paintEvent,
    keyPressEvent,
    timerEvent,
    _state,
    _actual,
    _level,
    _score,
    _final,
    _timer,
    _nextPieceLabel,
    _nextPiece,
    newPiece,
    start,
    drawSquare,
    squareWidth,
    squareHeight,
    GameState (Created, Running, Paused, GameOver)
) 
where

import TetrixPiece
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import System.Random

boardWidth :: Float
boardWidth = 10

boardHeight :: Float
boardHeight = 22

windowHeight :: Float
windowHeight = 660

windowWidth :: Float
windowWidth = 300

shapeToColor :: Shape -> Color
shapeToColor NoShape        = makeColorI 0 0 0 255
shapeToColor ZShape         = makeColorI 204 102 102 255
shapeToColor SShape         = makeColorI 102 204 102 255
shapeToColor LineShape      = makeColorI 102 102 204 255
shapeToColor TShape         = makeColorI 204 204 102 255
shapeToColor SquareShape    = makeColorI 204 102 204 255
shapeToColor LShape         = makeColorI 102 204 204 255
shapeToColor MirroredLShape = makeColorI 218 170 0 255

data GameState = Created | Running | Paused | GameOver deriving Eq

isStarted :: TetrixBoard -> Bool
isStarted board = _state board == Running || _state board == Paused

isPaused :: TetrixBoard -> Bool
isPaused board = _state board == Paused

newtype Score = MkScore Int deriving (Eq, Ord, Num)

newtype Level = MkLevel Int deriving (Eq, Ord, Num)

newtype X = MkX Int deriving (Eq, Ord, Num)

newtype Y = MkY Int deriving (Eq, Ord, Num)

instance Show Score where
    show (MkScore n) = show n 

instance Show Level where
    show (MkLevel n) = show n 

instance Show X where
    show (MkX n) = show n 

instance Show Y where
    show (MkY n) = show n 

unX :: X -> Int
unX (MkX n) = n

unY :: Y -> Int
unY (MkY n) = n

data Timer = Timer {
    _final ::  Float,
    _actual :: Float,
    _isTimerPaused :: Bool,
    _isTimerCounting :: Bool
}

createTimer :: Timer
createTimer = Timer {
    _final = 60,
    _actual = 0,
    _isTimerPaused = True,
    _isTimerCounting = False
}

startTimer :: Timer -> Timer
startTimer timer = timer { _actual = 0, _isTimerPaused = False, _isTimerCounting = True }

stopTimer :: Timer -> Timer
stopTimer timer = timer { _actual = 0, _isTimerPaused = True, _isTimerCounting = False}

setTimerFinal :: Timer -> Float -> Timer
setTimerFinal timer time = timer { _final = time }

data TetrixBoard = TetrixBoard {
    _state :: GameState,
    _gameOverLabel :: Picture,
    _startGameLabel :: Picture,
    _pausedLabel :: Picture,
    _timer :: Timer,
    _nextPieceLabel :: Maybe Picture,
    _isWaitingAfterLine :: Bool,
    _curPiece :: TetrixPiece,
    _nextPiece :: TetrixPiece,
    _curX :: X,
    _curY :: Y,
    _numLinesRemoved :: Int,
    _numPiecesDropped :: Int,
    _score :: Score,
    _level :: Level,
    _board :: [Shape],
    -- _board2 :: Board,
    -- equivalent to setFrameStyle
    _frameStyle :: Picture,
    -- TODO see whats equivalent to focusPolicy, maybe related do EventHandling
    _stdGen :: StdGen
}

createBoard :: StdGen -> Picture -> Picture -> Picture -> TetrixBoard
createBoard gen gameOverLabel pausedLabel startGameLabel = TetrixBoard {
    _state = Created,
    _timer = createTimer, 
    _nextPieceLabel = Nothing, 
    _gameOverLabel = gameOverLabel,
    _startGameLabel = startGameLabel,
    _pausedLabel = pausedLabel,
    _isWaitingAfterLine = False, 
    _curPiece = createPiece, 
    _nextPiece = piece, 
    _curX = 0, 
    _curY = 0, 
    _numLinesRemoved = 0, 
    _numPiecesDropped = 0, 
    _score = 0, 
    _level = 0, 
    _board = [NoShape | _ <- [0..(boardWidth * boardHeight - 1)]], 
    _frameStyle = rectangleSolid boardWidth boardHeight, 
    _stdGen = newGen
    }
    where
    (piece, newGen) = setRandomShape createPiece gen

resetBoard :: TetrixBoard -> TetrixBoard 
resetBoard board = board {
    _state = Created,
    _timer = createTimer,
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
    _board = [NoShape | _ <- [0..(boardWidth * boardHeight - 1)]], 
    _frameStyle = rectangleSolid boardWidth boardHeight, 
    _stdGen = newGen
}
    where
        (piece, newGen) = setRandomShape createPiece (_stdGen board)

shapeAt :: TetrixBoard -> X -> Y -> Shape
shapeAt board (MkX xCoord) (MkY yCoord) = _board board !! (yCoord * round boardWidth + xCoord)

setShapeAt :: TetrixBoard -> X -> Y -> Shape -> TetrixBoard
setShapeAt board (MkX xCoord) (MkY yCoord) shape = board { _board = newBoard }
    where
        targetIndex = (yCoord * round boardWidth) + xCoord
        newBoard = [
            if index == targetIndex
                then shape 
                else oldShape | (index, oldShape) <- zip [0..] (_board board) 
            ]

timeoutTime :: TetrixBoard -> Float
timeoutTime board = 120 / (1 + fromIntegral n)
    where MkLevel n = _level board

squareWidth :: Float
squareWidth  = windowWidth / boardWidth

squareHeight :: Float
squareHeight = windowHeight / boardHeight

setNextPieceLabel :: TetrixBoard -> Picture -> TetrixBoard
setNextPieceLabel board label = board { _nextPieceLabel = Just label}

clearBoard :: TetrixBoard -> TetrixBoard
clearBoard board = board { _board = [NoShape | _ <- [0..(boardHeight * boardWidth)]]}

start :: TetrixBoard -> TetrixBoard
start board = finalBoard
    where
        finalBoard = 
            if isPaused board
                then board
                else board3 
                    where
                        board0 = board {
                            _state = Running,
                            _isWaitingAfterLine = False,
                            _numLinesRemoved = 0,
                            _numPiecesDropped = 0,
                            _score = 0,
                            _level = 1
                        }
                        board1 = clearBoard board0
                        board2 = newPiece board1
                        timer = _timer board2
                        board3 = board2 { 
                            _timer = timer { _actual = 0, _final = timeoutTime board2}
                        }



pause :: TetrixBoard -> TetrixBoard
pause board = finalBoard
    where
        finalBoard = 
            if not (isStarted board)
                then board
                else board1
                where
                    timer = _timer board
                    board1 = 
                        if isPaused board
                            then board {  _state = Running, _timer = timer { _isTimerPaused = False, _isTimerCounting = True, _final = timeoutTime board }}
                            else board {  _state = Paused, _timer = timer { _isTimerCounting = False, _isTimerPaused = True}}

keyPressEvent :: Event -> TetrixBoard -> TetrixBoard
keyPressEvent (EventKey key Down _ _) board = finalBoard
    where
        (finalBoard, _) = 
            case (_state board) of
                Created -> 
                    case key of 
                        Char 's' -> (start board, False)
                        _        -> (board, False)
                Running ->
                    case key of
                        SpecialKey KeyLeft  -> tryMove board (_curPiece board) (_curX board - 1) (_curY board)
                        SpecialKey KeyRight -> tryMove board (_curPiece board) (_curX board + 1) (_curY board)
                        SpecialKey KeyUp    -> tryMove board (rotateRight $ _curPiece board) (_curX board) (_curY board)
                        SpecialKey KeyDown  -> tryMove board (rotateLeft $ _curPiece board) (_curX board) (_curY board)
                        SpecialKey KeySpace -> (dropDown board, False)
                        Char 'd'            -> (oneLineDown board, False)
                        Char 'p'            -> (pause board, False)
                        _                   -> (board, False)

                Paused ->
                    case key of
                        Char 'p'            -> (pause board, False)
                        _                   -> (board, False)

                GameOver ->
                    case key of
                        Char 's'            -> (start (resetBoard board), False)
                        _                   -> (board, False)


keyPressEvent _ board                       = board
        

paintEvent :: TetrixBoard -> Picture
paintEvent board = Pictures [finalPicture]
    where
        left   = -(windowWidth / 2)
        bottom = -(windowHeight / 2)

        initialPicture =  rectangleSolid windowWidth windowHeight
        pausedOverflow = Pictures [
                color (makeColorI 0 0 0 150) $ rectangleSolid windowWidth windowHeight,
                scale 2 2 $ _pausedLabel board
            ]

        gameOverOverflow = Pictures [
                color (makeColorI 0 0 0 150) $ rectangleSolid windowWidth windowHeight,
                translate 0 50 $ scale 2 2 $ _gameOverLabel board
            ]

        startNewGameOverflow = Pictures [
                color (makeColorI 0 0 0 150) $ rectangleSolid windowWidth windowHeight,
                translate 0 50 $ scale 2 2 $ _startGameLabel board
            ]

        finalPicture = 
            case (_state board) of
                Created  -> Pictures [finalDraw, startNewGameOverflow]
                Paused   -> Pictures [finalDraw, pausedOverflow]
                GameOver -> Pictures [finalDraw, gameOverOverflow]
                _        -> finalDraw

            where 
                finalDraw = 
                    if (_shape $ _curPiece board) /= NoShape
                        then Pictures [initialPicture, drawPictures rowList, drawCurPiece [0..3]]
                        else Pictures [initialPicture, drawPictures rowList]

        columnList :: [Int]
        columnList = [0..round boardWidth - 1]

        rowList :: [Int]
        rowList = [0..round boardHeight - 1]

        drawPictures :: [Int] -> Picture
        drawPictures []         = blank
        drawPictures (r:rows)   = Pictures [(drawPic r columnList), drawPictures rows]
            where
                drawPic :: Int -> [Int] -> Picture
                drawPic _ []                 = blank
                drawPic row (column:columns) = Pictures [positionedSquare, drawPic row columns]
                    where
                        positionedSquare = 
                            drawSquare (round xCoord) (round yCoord) actualShape
                                where
                                    xCoord = left + fromIntegral column * squareWidth
                                    yCoord = bottom + fromIntegral row * squareHeight
                                    actualShape = shapeAt board (MkX column) (MkY row) 

        drawCurPiece :: [Int] -> Picture
        drawCurPiece []     = blank
        drawCurPiece (b:bs) = Pictures [newP, drawCurPiece bs] 
            where
                newP = drawSquare (round xCoord) (round yCoord) actualShape
                    where
                        curX = _curX board + MkX (x (_curPiece board) b)
                        curY = _curY board + MkY (y (_curPiece board) b)

                        xCoord = left + (fromIntegral (unX curX) * squareWidth)
                        -- yCoord = bottom + (boardHeight - fromIntegral curY - 1) * squareHeight
                        yCoord = bottom + (fromIntegral (unY curY) * squareHeight)
                        actualShape = _shape $ _curPiece board
                        -- actualShape = ZShape


timerEvent :: TetrixBoard -> TetrixBoard
timerEvent board = finalBoard
    where
        finalBoard = 
            if _isWaitingAfterLine board
                then board2
                else oneLineDown board
                where
                    board0 = board { _isWaitingAfterLine = False }
                    board1 = newPiece board0
                    timer = _timer board1
                    timer0 = startTimer timer
                    timer1 = setTimerFinal timer0 (timeoutTime board1)
                    board2 = board1 { _timer = timer1 }


dropDown :: TetrixBoard -> TetrixBoard
dropDown board = finalBoard 
    where
        dropHeight = 0
        newY = _curY board

        incDropHeight :: TetrixBoard -> Y -> Int -> (TetrixBoard, Int)
        incDropHeight b 0 dh  = (b, dh)
        incDropHeight b ny dh = 
            if canMove
                then incDropHeight board0 (ny - 1) (dh + 1)
                else (b, dh) 
                where
                    (board0, canMove) = tryMove b (_curPiece board) (_curX board) (ny - 1)

        (board1, finalDropHeight) = incDropHeight board newY dropHeight
        finalBoard = pieceDropped board1 finalDropHeight


oneLineDown :: TetrixBoard -> TetrixBoard
oneLineDown board = finalBoard
    where
        (board0, moved) = tryMove board (_curPiece board) (_curX board) ((_curY board) - 1)
        finalBoard = 
            if not moved
                then pieceDropped board0 0
                else board0

pieceDropped :: TetrixBoard -> Int -> TetrixBoard
pieceDropped board dropHeight = finalBoard
    where
        processDrop :: TetrixBoard -> [Int] -> TetrixBoard
        processDrop tb []               = tb
        processDrop tb (square:squares) = processDrop ntb squares
            where
                xCoord = _curX tb + MkX (x (_curPiece tb) square)
                yCoord = _curY tb + MkY (y (_curPiece tb) square)
                ntb = setShapeAt tb xCoord yCoord (_shape (_curPiece tb))

        board0 = processDrop board [0..3]

        board1 = board0 { _numPiecesDropped = _numPiecesDropped board + 1}

        board2 = 
            if _numPiecesDropped board1 `mod` 25 == 0
                then board1 {
                    _level = _level board1 + 1,
                    _timer = startTimer (_timer board1)
                    -- TODO emit level changed
                }
                else board1
        board3 = board2 {
            _score = _score board2 + MkScore dropHeight + 7
            -- TODO emit score changed
        }

        board4 = removeFullLines board3
        finalBoard = 
            if not (_isWaitingAfterLine board4)
                then newPiece board4
                else board4

removeFullLines :: TetrixBoard -> TetrixBoard
removeFullLines board = finalBoard
    where
        finalBoard = 
            if nfl > 0
                then b {
                    _numLinesRemoved = _numLinesRemoved b + nfl,
                    _score = _score b + 10 * MkScore nfl,
                    -- _timer = (_timer b) { _final = 60, _actual = 0 },
                    _isWaitingAfterLine = True,
                    _curPiece = setShape (_curPiece b) NoShape
                }
                else
                    b

        columnList :: [Int]
        columnList = [0 .. round boardWidth - 1]

        rowList :: [Int]
        rowList = reverse [0.. round boardHeight - 1]

        (b, nfl) = processRows board 0 rowList

        processRows :: TetrixBoard -> Int -> [Int] -> (TetrixBoard, Int)
        processRows tb numFullLines []         = (tb, numFullLines)
        processRows tb numFullLines (row:rows) = processRows newBoard newFullLines rows
            where
                isRowFull :: Int -> [Int] -> Bool
                isRowFull _ []               = True
                isRowFull yCoord (xCoord:xs) =
                    if actualShape == NoShape then False else isRowFull yCoord xs 
                        where
                            actualShape = shapeAt tb (MkX xCoord) (MkY yCoord)

                rowIsFull = isRowFull row columnList

                newFullLines = 
                    if rowIsFull 
                        then numFullLines + 1 
                        else numFullLines

                newBoard = 
                    if rowIsFull 
                        then clearRow (updateRows tb [row..round boardHeight - 2]) (round boardHeight - 1) columnList 
                        else tb

                clearRow :: TetrixBoard -> Int -> [Int] -> TetrixBoard
                clearRow board1 _ [] = board1
                clearRow board1 yCoord (xCoord:xs) = clearRow nextBoard yCoord xs
                    where
                        nextBoard = setShapeAt board1 (MkX xCoord) (MkY yCoord) NoShape

                updateRows :: TetrixBoard -> [Int] -> TetrixBoard
                updateRows b0 []     = b0
                updateRows b0 (r:rs) = updateRows (updateRow b0 r columnList) rs 

                updateRow :: TetrixBoard -> Int -> [Int] -> TetrixBoard
                updateRow board1 _ [] = board1
                updateRow board1 yCoord (xCoord:xs) = updateRow nextBoard yCoord xs 
                    where
                        nextBoard = setShapeAt board1 (MkX xCoord) (MkY yCoord) upperShape 
                            where
                                upperShape = shapeAt board1 (MkX xCoord) (MkY (yCoord + 1))

newPiece :: TetrixBoard -> TetrixBoard
newPiece board = board4
    where
        (nextPiece, newGen) = setRandomShape (_nextPiece board) (_stdGen board)
        board1 = board {
            _curPiece = _nextPiece board,
            _stdGen = newGen
        }
        board2 = showNextPiece board1
        board3 = board2 {
            _curX = MkX (truncate (boardWidth / 2) - 1),
            _curY = MkY (round boardHeight - 1 - maxY (_curPiece board2))
        }
        board4 = 
            if not (snd (tryMove board3 (_curPiece board3) (_curX board3) (_curY board3)))
                then board3 {
                    _curPiece = setShape (_curPiece board3) NoShape,
                    _timer = stopTimer (_timer board3),
                    _state = GameOver
                }
                else board3 {
                    _nextPiece = nextPiece
                }

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

tryMove :: TetrixBoard -> TetrixPiece -> X -> Y -> (TetrixBoard, Bool)
tryMove board curPiece newX newY = (finalBoard, isValidNextPos) 
    where
        validateNextPos _ _ _ _ []         = True 
        validateNextPos b0 cp x0 y0 (i:is) =
            if isOutOfBonds || destinyHasShape
                then False
                else validateNextPos b0 cp x0 y0 is
                where
                    isOutOfBonds = getX i < 0 || getX i >= MkX (round boardWidth) || getY i < 0 || getY i >= MkY (round boardHeight) 
                    destinyHasShape = shapeAt b0 (getX i) (getY i) /= NoShape

                    getX squareX = x0 + MkX (x cp squareX)
                    getY squareY = y0 + MkY (y cp squareY)


        isValidNextPos = validateNextPos board curPiece newX newY [0..3]
        finalBoard = 
            if isValidNextPos
                then board {
                    _curPiece= curPiece,
                    _curX = newX,
                    _curY = newY
                }
                else board

drawSquare :: Int -> Int -> Shape -> Picture
drawSquare xCoord yCoord shape = pictures [outerSquare, centerSquare]
    where
        baseColor = shapeToColor shape
        centerSquare = 
            color baseColor $
            translate centerX centerY $ 
            rectangleSolid (squareWidth - 2) (squareHeight - 2)

        outerSquare =
            color (light baseColor) $
            translate centerX centerY $ 
            rectangleSolid (squareWidth) (squareHeight)

        centerX = fromIntegral xCoord + squareWidth / 2
        centerY = fromIntegral yCoord + squareHeight / 2

