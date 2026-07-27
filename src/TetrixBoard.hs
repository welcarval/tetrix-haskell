{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RecordWildCards #-}

module TetrixBoard (
    TetrixBoard,
    SomeTetrixBoard (SomeTetrixBoard),
    SGameState (SCreated, SRunning, SPaused, SGameOver),
    createBoard, 
    paintEvent,
    keyPressEvent,
    advanceTimer,
    _level,
    _score,
    _nextPiece,
    newPiece,
    drawSquare,
    squareWidth,
    squareHeight,
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

data GameState = Created | Running | Paused | GameOver

data SGameState (s :: GameState) where
    SCreated  :: SGameState 'Created
    SRunning  :: SGameState 'Running
    SPaused   :: SGameState 'Paused
    SGameOver :: SGameState 'GameOver

data SomeTetrixBoard where
    SomeTetrixBoard :: SGameState s -> TetrixBoard s -> SomeTetrixBoard

newtype Score = Score Int deriving (Eq, Ord, Num, Show)

newtype Level = Level Int deriving (Eq, Ord, Num, Show)

newtype X = X Int deriving (Eq, Ord, Num, Show)

newtype Y = Y Int deriving (Eq, Ord, Num, Show)

class Unwrap a where
    toInt :: a -> Int

instance Unwrap X where
    toInt (X n) = n

instance Unwrap Y where
    toInt (Y n) = n

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

data TetrixBoard (s :: GameState) = TetrixBoard {
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

-- TODO, used just to recreate board changing his gameState, maybe should use another idea
-- for this behavior

retagBoard :: TetrixBoard s -> TetrixBoard s'
retagBoard TetrixBoard{..} = TetrixBoard{..}

createBoard :: StdGen -> Picture -> Picture -> Picture -> TetrixBoard 'Created
createBoard gen gameOverLabel pausedLabel startGameLabel = TetrixBoard {
    -- _state = Created,
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

resetBoard :: TetrixBoard s -> TetrixBoard 'Created 
resetBoard board = retagBoard board {
    -- _state = Created,
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

shapeAt :: TetrixBoard s -> X -> Y -> Shape
shapeAt board (X xCoord) (Y yCoord) = _board board !! (yCoord * round boardWidth + xCoord)

setShapeAt :: TetrixBoard s -> X -> Y -> Shape -> TetrixBoard s
setShapeAt board (X xCoord) (Y yCoord) shape = board { _board = newBoard }
    where
        targetIndex = (yCoord * round boardWidth) + xCoord
        newBoard = [
            if index == targetIndex
                then shape 
                else oldShape | (index, oldShape) <- zip [0..] (_board board) 
            ]

timeoutTime :: TetrixBoard s -> Float
timeoutTime board = 120 / (1 + fromIntegral n)
    where Level n = _level board

squareWidth :: Float
squareWidth  = windowWidth / boardWidth

squareHeight :: Float
squareHeight = windowHeight / boardHeight

setNextPieceLabel :: TetrixBoard s -> Picture -> TetrixBoard s
setNextPieceLabel board label = board { _nextPieceLabel = Just label}

clearBoard :: TetrixBoard s -> TetrixBoard s
clearBoard board = board { _board = [NoShape | _ <- [0..(boardHeight * boardWidth)]]}

start :: TetrixBoard 'Created -> SomeTetrixBoard
start board =
    case newPiece boardRunning of
        SomeTetrixBoard witness b -> SomeTetrixBoard witness (applyStartTimer b)
    where
        board0 = board {
            _isWaitingAfterLine = False,
            _numLinesRemoved = 0,
            _numPiecesDropped = 0,
            _score = 0,
            _level = 1
        }
        board1 = clearBoard board0
        boardRunning = retagBoard board1

        applyStartTimer :: TetrixBoard s' -> TetrixBoard s'
        applyStartTimer b = b { _timer = timer { _actual = 0, _final = timeoutTime b } }
            where timer = _timer b
    
    -- where
    --     finalBoard = 
    --         if isPaused board
    --             then board
    --             else board3 
    --                 where
    --                     board0 = board {
    --                         -- _state = Running,
    --                         _isWaitingAfterLine = False,
    --                         _numLinesRemoved = 0,
    --                         _numPiecesDropped = 0,
    --                         _score = 0,
    --                         _level = 1
    --                     }
    --                     board1 = clearBoard board0
    --                     board2 = newPiece board1
    --                     timer = _timer board2
    --                     board3 = board2 { 
    --                         _timer = timer { _actual = 0, _final = timeoutTime board2}
    --                     }

pause :: TetrixBoard 'Running -> TetrixBoard 'Paused
pause board = retagBoard board { _timer = timer { _isTimerCounting = False, _isTimerPaused = True } }
    where timer = _timer board
    -- where
    --     finalBoard = 
    --         if not (isStarted board)
    --             then board
    --             else board1
    --             where
    --                 timer = _timer board
    --                 board1 = 
    --                     if isPaused board
    --                         then board {  _timer = timer { _isTimerPaused = False, _isTimerCounting = True, _final = timeoutTime board }}
    --                         else board {  _timer = timer { _isTimerCounting = False, _isTimerPaused = True}}
    --

resume :: TetrixBoard 'Paused -> TetrixBoard 'Running
resume board = retagBoard board { _timer = timer { _isTimerPaused = False, _isTimerCounting = True, _final = timeoutTime board } }
    where timer = _timer board

keyPressEvent :: Event -> SomeTetrixBoard -> SomeTetrixBoard
keyPressEvent (EventKey key Down _ _) (SomeTetrixBoard witness board) =
    case witness of
        SCreated ->
            case key of
                Char 's' -> start board
                _        -> SomeTetrixBoard SCreated board
        SRunning ->
            case key of
                SpecialKey KeyLeft  -> asRunning $ fst $ tryMove board (_curPiece board) (_curX board - 1) (_curY board)
                SpecialKey KeyRight -> asRunning $ fst $ tryMove board (_curPiece board) (_curX board + 1) (_curY board)
                SpecialKey KeyUp    -> asRunning $ fst $ tryMove board (rotateRight $ _curPiece board) (_curX board) (_curY board)
                SpecialKey KeyDown  -> asRunning $ fst $ tryMove board (rotateLeft $ _curPiece board) (_curX board) (_curY board)
                SpecialKey KeySpace -> dropDown board
                Char 'd'            -> oneLineDown board
                Char 'p'            -> asPaused $ pause board
                _                   -> asRunning board
                    
            where
                asRunning = SomeTetrixBoard SRunning
                asPaused  = SomeTetrixBoard SPaused
                
        SPaused ->
            case key of
                Char 'p' -> SomeTetrixBoard SRunning (resume board)
                _        -> SomeTetrixBoard SPaused board

        SGameOver ->
            case key of
                Char 's' -> start $ resetBoard board
                _        -> SomeTetrixBoard SGameOver board

keyPressEvent _ someBoard = someBoard

--     where
--         (finalBoard, _) = 
--             case (_state board) of
--                 Created -> 
--                     case key of 
--                         Char 's' -> (start board, False)
--                         _        -> (board, False)
--                 Running ->
--                     case key of
--                         SpecialKey KeyLeft  -> tryMove board (_curPiece board) (_curX board - 1) (_curY board)
--                         SpecialKey KeyRight -> tryMove board (_curPiece board) (_curX board + 1) (_curY board)
--                         SpecialKey KeyUp    -> tryMove board (rotateRight $ _curPiece board) (_curX board) (_curY board)
--                         SpecialKey KeyDown  -> tryMove board (rotateLeft $ _curPiece board) (_curX board) (_curY board)
--                         SpecialKey KeySpace -> (dropDown board, False)
--                         Char 'd'            -> (oneLineDown board, False)
--                         Char 'p'            -> (pause board, False)
--                         _                   -> (board, False)
--
--                 Paused ->
--                     case key of
--                         Char 'p'            -> (pause board, False)
--                         _                   -> (board, False)
--
--                 GameOver ->
--                     case key of
--                         Char 's'            -> (start (resetBoard board), False)
--                         _                   -> (board, False)
--
--
-- keyPressEvent _ board                       = board
--
--
overlayFor :: SGameState s -> TetrixBoard s -> Maybe Picture
overlayFor SCreated board = Just (Pictures [
        color (makeColorI 0 0 0 150) $ rectangleSolid windowWidth windowHeight,
        translate 0 50 $ scale 2 2 $ _startGameLabel board
    ])
overlayFor SRunning _ = Nothing
overlayFor SPaused board = Just (Pictures [
        color (makeColorI 0 0 0 150) $ rectangleSolid windowWidth windowHeight,
        scale 2 2 $ _pausedLabel board
    ])
overlayFor SGameOver board = Just (Pictures [
        color (makeColorI 0 0 0 150) $ rectangleSolid windowWidth windowHeight,
        translate 0 50 $ scale 2 2 $ _gameOverLabel board
    ])

paintEvent :: SomeTetrixBoard -> Picture
paintEvent (SomeTetrixBoard witness board) =
    case overlayFor witness board of
        Just overlay -> Pictures [renderBoard board, overlay]
        Nothing      -> renderBoard board


renderBoard :: TetrixBoard s -> Picture
renderBoard board = finalDraw
    where
        left   = -(windowWidth / 2)
        bottom = -(windowHeight / 2)

        initialPicture =  rectangleSolid windowWidth windowHeight
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
                                    actualShape = shapeAt board (X column) (Y row) 

        drawCurPiece :: [Int] -> Picture
        drawCurPiece []     = blank
        drawCurPiece (b:bs) = Pictures [newP, drawCurPiece bs] 
            where
                newP = drawSquare (round xCoord) (round yCoord) actualShape
                    where
                        curX = _curX board + X (x (_curPiece board) b)
                        curY = _curY board + Y (y (_curPiece board) b)

                        xCoord = left + (fromIntegral (toInt curX) * squareWidth)
                        -- yCoord = bottom + (boardHeight - fromIntegral curY - 1) * squareHeight
                        yCoord = bottom + (fromIntegral (toInt curY) * squareHeight)
                        actualShape = _shape $ _curPiece board
                        -- actualShape = ZShape


advanceTimer :: SomeTetrixBoard -> SomeTetrixBoard
advanceTimer (SomeTetrixBoard SRunning board) =
    if _actual timer1 >= _final timer1
        then timerEvent (board { _timer = timer1 { _actual = 0 } })
        else SomeTetrixBoard SRunning (board { _timer = timer1 })
    where
        timer0 = _timer board
        timer1 = timer0 { _actual = _actual timer0 + 1}
advanceTimer someBoard = someBoard

timerEvent :: TetrixBoard 'Running -> SomeTetrixBoard
timerEvent board =
    if _isWaitingAfterLine board
        then case newPiece board0 of
            SomeTetrixBoard witness b -> SomeTetrixBoard witness (restartTimer b)
        else oneLineDown board
    where
        board0 = board { _isWaitingAfterLine = False }

        restartTimer :: TetrixBoard s' -> TetrixBoard s'
        restartTimer b = b { _timer = setTimerFinal (startTimer (_timer b)) (timeoutTime b)}

        -- board1 = newPiece board0
        -- timer = _timer board1
        -- timer0 = startTimer timer
        -- timer1 = setTimerFinal timer0 (timeoutTime board1)
        -- board2 = board1 { _timer = timer1 }

dropDown :: TetrixBoard 'Running -> SomeTetrixBoard
dropDown board = pieceDropped board1 finalDropHeight 
    where
        dropHeight = 0
        newY = _curY board

        incDropHeight :: TetrixBoard 'Running -> Y -> Int -> (TetrixBoard 'Running, Int)
        incDropHeight b 0 dh  = (b, dh)
        incDropHeight b ny dh = 
            if canMove
                then incDropHeight board0 (ny - 1) (dh + 1)
                else (b, dh) 
                where
                    (board0, canMove) = tryMove b (_curPiece board) (_curX board) (ny - 1)

        (board1, finalDropHeight) = incDropHeight board newY dropHeight

oneLineDown :: TetrixBoard 'Running -> SomeTetrixBoard
oneLineDown board = finalBoard
    where
        (board0, moved) = tryMove board (_curPiece board) (_curX board) ((_curY board) - 1)
        finalBoard = 
            if not moved
                then pieceDropped board0 0
                else SomeTetrixBoard SRunning board0

pieceDropped :: TetrixBoard 'Running -> Int -> SomeTetrixBoard
pieceDropped board dropHeight = finalBoard
    where
        processDrop :: TetrixBoard 'Running -> [Int] -> TetrixBoard 'Running
        processDrop tb []               = tb
        processDrop tb (square:squares) = processDrop ntb squares
            where
                xCoord = _curX tb + X (x (_curPiece tb) square)
                yCoord = _curY tb + Y (y (_curPiece tb) square)
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
            _score = _score board2 + Score dropHeight + 7
            -- TODO emit score changed
        }

        board4 = removeFullLines board3
        finalBoard = 
            if not (_isWaitingAfterLine board4)
                then newPiece board4
                else SomeTetrixBoard SRunning board4

removeFullLines :: TetrixBoard s -> TetrixBoard s
removeFullLines board = finalBoard
    where
        finalBoard = 
            if nfl > 0
                then b {
                    _numLinesRemoved = _numLinesRemoved b + nfl,
                    _score = _score b + 10 * Score nfl,
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

        processRows :: TetrixBoard s -> Int -> [Int] -> (TetrixBoard s, Int)
        processRows tb numFullLines []         = (tb, numFullLines)
        processRows tb numFullLines (row:rows) = processRows newBoard newFullLines rows
            where
                isRowFull :: Int -> [Int] -> Bool
                isRowFull _ []               = True
                isRowFull yCoord (xCoord:xs) =
                    if actualShape == NoShape then False else isRowFull yCoord xs 
                        where
                            actualShape = shapeAt tb (X xCoord) (Y yCoord)

                rowIsFull = isRowFull row columnList

                newFullLines = 
                    if rowIsFull 
                        then numFullLines + 1 
                        else numFullLines

                newBoard = 
                    if rowIsFull 
                        then clearRow (updateRows tb [row..round boardHeight - 2]) (round boardHeight - 1) columnList 
                        else tb

                clearRow :: TetrixBoard s -> Int -> [Int] -> TetrixBoard s
                clearRow board1 _ [] = board1
                clearRow board1 yCoord (xCoord:xs) = clearRow nextBoard yCoord xs
                    where
                        nextBoard = setShapeAt board1 (X xCoord) (Y yCoord) NoShape

                updateRows :: TetrixBoard s -> [Int] -> TetrixBoard s
                updateRows b0 []     = b0
                updateRows b0 (r:rs) = updateRows (updateRow b0 r columnList) rs 

                updateRow :: TetrixBoard s -> Int -> [Int] -> TetrixBoard s
                updateRow board1 _ [] = board1
                updateRow board1 yCoord (xCoord:xs) = updateRow nextBoard yCoord xs 
                    where
                        nextBoard = setShapeAt board1 (X xCoord) (Y yCoord) upperShape 
                            where
                                upperShape = shapeAt board1 (X xCoord) (Y (yCoord + 1))

newPiece :: TetrixBoard 'Running -> SomeTetrixBoard
newPiece board = board4
    where
        (nextPiece, newGen) = setRandomShape (_nextPiece board) (_stdGen board)
        board1 = board {
            _curPiece = _nextPiece board,
            _stdGen = newGen
        }
        board2 = showNextPiece board1
        board3 = board2 {
            _curX = X (truncate (boardWidth / 2) - 1),
            _curY = Y (round boardHeight - 1 - maxY (_curPiece board2))
        }
        board4 = 
            if not (snd (tryMove board3 (_curPiece board3) (_curX board3) (_curY board3)))
                then SomeTetrixBoard SGameOver (retagBoard board3 {
                    _curPiece = setShape (_curPiece board3) NoShape,
                    _timer = stopTimer (_timer board3)
                    -- _state = GameOver
                })
                else SomeTetrixBoard SRunning (board3 {
                    _nextPiece = nextPiece
                })

showNextPiece :: TetrixBoard s -> TetrixBoard s
showNextPiece board = 
    case _nextPieceLabel board of
        Just _  -> board
        Nothing -> board { _nextPieceLabel = Just (Pictures squares)}
    where
        -- dx = maxX nextPiece - minX nextPiece
        -- dy = maxY nextPiece - minY nextPiece
        nextPiece = _nextPiece board
        squares = [drawSquare (x nextPiece i) (y nextPiece i) (_shape nextPiece) | i <- [0..3]]

tryMove :: TetrixBoard s -> TetrixPiece -> X -> Y -> (TetrixBoard s, Bool)
tryMove board curPiece newX newY = (finalBoard, isValidNextPos) 
    where
        validateNextPos _ _ _ _ []         = True 
        validateNextPos b0 cp x0 y0 (i:is) =
            if isOutOfBonds || destinyHasShape
                then False
                else validateNextPos b0 cp x0 y0 is
                where
                    isOutOfBonds = getX i < 0 || getX i >= X (round boardWidth) || getY i < 0 || getY i >= Y (round boardHeight) 
                    destinyHasShape = shapeAt b0 (getX i) (getY i) /= NoShape

                    getX squareX = x0 + X (x cp squareX)
                    getY squareY = y0 + Y (y cp squareY)

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

