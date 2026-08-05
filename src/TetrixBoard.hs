{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE RecordWildCards            #-}

module TetrixBoard (
    TetrixBoard,
    SomeTetrixBoard (SomeTetrixBoard),
    SGameState (SCreated, SRunning, SPaused, SGameOver),
    BoardLabels (..),
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

import           Data.Maybe                         (fromMaybe)
import           Graphics.Gloss
import           Graphics.Gloss.Interface.Pure.Game
import           System.Random
import           TetrixPiece

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

newtype Score = Score Int deriving (Eq, Ord, Num)

newtype Level = Level Int deriving (Eq, Ord, Num)

newtype X = X Int deriving (Eq, Ord, Num, Show)

newtype Y = Y Int deriving (Eq, Ord, Num, Show)

class Unwrap a where
    toInt :: a -> Int

instance Unwrap X where
    toInt (X n) = n

instance Unwrap Y where
    toInt (Y n) = n

instance Show Score where
    show (Score n) = show n

instance Show Level where
    show (Level n) = show n

data Timer = Timer {
    _final           ::  Float,
    _actual          :: Float
}

createTimer :: Timer
createTimer = Timer {
    _final = 60,
    _actual = 0
}

resetTimer :: Timer -> Timer
resetTimer timer = timer { _actual = 0 }

setTimerFinal :: Timer -> Float -> Timer
setTimerFinal timer time = timer { _final = time }

data TetrixBoard (s :: GameState) = TetrixBoard {
    _gameOverLabel      :: Picture,
    _startGameLabel     :: Picture,
    _pausedLabel        :: Picture,
    _timer              :: Timer,
    _nextPieceLabel     :: Maybe Picture,
    _isWaitingAfterLine :: Bool,
    _curPiece           :: TetrixPiece,
    _nextPiece          :: TetrixPiece,
    _curX               :: X,
    _curY               :: Y,
    _numLinesRemoved    :: Int,
    _numPiecesDropped   :: Int,
    _score              :: Score,
    _level              :: Level,
    _board              :: [Shape],
    _frameStyle         :: Picture,
    _stdGen             :: StdGen
}

retagBoard :: TetrixBoard s -> TetrixBoard s'
retagBoard TetrixBoard{..} = TetrixBoard{..}

data BoardLabels = BoardLabels {
    _overlayGameOver  :: Picture,
    _overlayPaused    :: Picture,
    _overlayStartGame :: Picture
}

createBoard :: StdGen -> BoardLabels -> TetrixBoard 'Created
createBoard gen labels = TetrixBoard {
    _timer = createTimer,
    _nextPieceLabel = Nothing,
    _gameOverLabel = _overlayGameOver labels,
    _startGameLabel = _overlayStartGame labels,
    _pausedLabel = _overlayPaused labels,
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

pause :: TetrixBoard 'Running -> TetrixBoard 'Paused
pause board = retagBoard board 

resume :: TetrixBoard 'Paused -> TetrixBoard 'Running
resume board = retagBoard board { _timer = timer { _final = timeoutTime board } }
    where timer = _timer board

keyPressEvent :: Event -> SomeTetrixBoard -> SomeTetrixBoard
keyPressEvent (EventKey key Down _ _) (SomeTetrixBoard witness board) =
    case witness of
        SCreated ->
            case key of
                Char 's' -> start board
                _        -> SomeTetrixBoard SCreated board
        SRunning ->
            if _isWaitingAfterLine board
                then case key of
                    Char 'p'            -> asPaused $ pause board
                    _                   -> asRunning board
                else case key of
                    SpecialKey KeyLeft  -> asRunning $ fromMaybe board $ tryMove board (_curPiece board) (_curX board - 1) (_curY board)
                    SpecialKey KeyRight -> asRunning $ fromMaybe board $ tryMove board (_curPiece board) (_curX board + 1) (_curY board)
                    SpecialKey KeyUp    -> asRunning $ fromMaybe board $ tryMove board (rotateRight $ _curPiece board) (_curX board) (_curY board)
                    SpecialKey KeyDown  -> asRunning $ fromMaybe board $ tryMove board (rotateLeft $ _curPiece board) (_curX board) (_curY board)
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
                        yCoord = bottom + (fromIntegral (toInt curY) * squareHeight)
                        actualShape = _shape $ _curPiece board

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
        restartTimer b = b { _timer = setTimerFinal (resetTimer (_timer b)) (timeoutTime b)}

dropDown :: TetrixBoard 'Running -> SomeTetrixBoard
dropDown board = pieceDropped board1 finalDropHeight
    where
        dropHeight = 0
        newY = _curY board

        incDropHeight :: TetrixBoard 'Running -> Y -> Int -> (TetrixBoard 'Running, Int)
        incDropHeight b 0 dh  = (b, dh)
        incDropHeight b ny dh =
            case tryMove b (_curPiece board) (_curX board) (ny - 1) of
                Just board0 -> incDropHeight board0 (ny - 1) (dh + 1)
                Nothing     -> (b, dh)

        (board1, finalDropHeight) = incDropHeight board newY dropHeight

oneLineDown :: TetrixBoard 'Running -> SomeTetrixBoard
oneLineDown board =
        case tryMove board (_curPiece board) (_curX board) ((_curY board) - 1) of
            Just board0 -> SomeTetrixBoard SRunning board0
            Nothing     -> pieceDropped board 0

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
                    _timer = resetTimer (_timer board1)
                }
                else board1
        board3 = board2 {
            _score = _score board2 + Score dropHeight + 7
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
            _curX = X $ truncate (boardWidth / 2) - 1,
            _curY = Y $ round boardHeight - 1 - maxY (_curPiece board2)
        }
        board4 =
            case tryMove board3 (_curPiece board3) (_curX board3) (_curY board3) of
                Nothing -> SomeTetrixBoard SGameOver (retagBoard board3 {
                    _curPiece = setShape (_curPiece board3) NoShape,
                    _timer = resetTimer (_timer board3)
                })
                Just _ -> SomeTetrixBoard SRunning (board3 {
                    _nextPiece = nextPiece
                })

showNextPiece :: TetrixBoard s -> TetrixBoard s
showNextPiece board =
    case _nextPieceLabel board of
        Just _  -> board
        Nothing -> board { _nextPieceLabel = Just (Pictures squares)}
    where
        nextPiece = _nextPiece board
        squares = [drawSquare (x nextPiece i) (y nextPiece i) (_shape nextPiece) | i <- [0..3]]

tryMove :: TetrixBoard s -> TetrixPiece -> X -> Y -> Maybe (TetrixBoard s)
tryMove board curPiece newX newY =
    if isValidNextPos
        then Just (board {
            _curPiece = curPiece,
            _curX     = newX,
            _curY     = newY
            })
        else Nothing
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
