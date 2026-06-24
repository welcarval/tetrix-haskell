module TetrixBoard (
    TetrixBoard,
    createBoard, 
    paintEvent
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

colorTable :: [[Int]]
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

data Timer = Timer {
    _final :: Maybe Float,
    _actual :: Float,
    _isTimerPaused :: Bool,
    _isTimerCounting :: Bool
}

createTimer :: Timer
createTimer = Timer {
    _final = Nothing,
    _actual = 0,
    _isTimerPaused = True,
    _isTimerCounting = False
}

startTimer :: Timer -> Timer
startTimer timer = timer { _actual = 0, _isTimerPaused = False, _isTimerCounting = True }

stopTimer :: Timer -> Timer
stopTimer timer = timer { _actual = 0, _isTimerPaused = True, _isTimerCounting = False}

setTimerFinal :: Timer -> Float -> Timer
setTimerFinal timer time = timer { _final = Just time }

data TetrixBoard = TetrixBoard {
    _timer :: Timer,
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

createBoard :: StdGen -> TetrixBoard
createBoard gen = TetrixBoard {
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

start :: TetrixBoard -> TetrixBoard
start board = finalBoard
    where
        finalBoard = 
            if _isPaused board
                then board
                else board3 
                    where
                        board0 = board {
                            _isStarted = True,
                            _isWaitingAfterLine = False,
                            _numLinesRemoved = 0,
                            _numPiecesDropped = 0,
                            _score = 0,
                            _level = 1
                        }
                        board1 = clearBoard board0
                        board2 = newPiece board1
                        board3 = board2 { 
                            _timer = startTimer (_timer board2) { _final = Just (timeoutTime board2)}
                            }



pause :: TetrixBoard -> TetrixBoard
pause board = finalBoard
    where
        finalBoard = 
            if _isStarted board
                then board
                else board { 
                    _isPaused = not (_isPaused board), 
                    _timer = if _isPaused board then stopTimer (_timer board) else startTimer (_timer board)
                }

keyPressEvent :: Event -> TetrixBoard -> TetrixBoard
keyPressEvent (EventKey key Down _ _) board = finalBoard
    where
        (finalBoard, _) = 
            if not (_isStarted board) || _isPaused board || (_shape $ _curPiece board) == NoShape
                then (board, False)
                else 
                    case key of
                        SpecialKey KeyLeft  -> tryMove board (_curPiece board) (_curX board - 1) (_curY board)
                        SpecialKey KeyRight -> tryMove board (_curPiece board) (_curX board + 1) (_curY board)
                        SpecialKey KeyDown  -> tryMove board (rotateRight $ _curPiece board) (_curX board + 1) (_curY board)
                        SpecialKey KeyUp    -> tryMove board (rotateLeft $ _curPiece board) (_curX board + 1) (_curY board)
                        SpecialKey KeySpace -> (dropDown board, False)
                        Char 'D'            -> (oneLineDown board, False)
                        _                   -> (board, False)



keyPressEvent _ board                       = board
        

paintEvent :: TetrixBoard -> Picture
paintEvent board = Pictures [finalPicture]
    where
        left   = -(windowWidth / 2)
        right  = windowWidth / 2
        top    = windowHeight / 2
        bottom = -(windowHeight / 2)

        initialPicture = translate left top $ rectangleSolid windowWidth windowHeight

        finalPicture = 
            if _isPaused board
                then text "pause"
                else 
                    if (_shape $ _curPiece board) /= NoShape
                        then Pictures [drawCurPiece initialPicture [0..3], drawPictures initialPicture rowList]
                        else drawPictures initialPicture rowList

        columnList :: [Int]
        columnList = [0..round boardWidth - 1]

        rowList :: [Int]
        rowList = [0..round boardHeight - 1]

        drawPictures :: Picture -> [Int] -> Picture
        drawPictures p []         = p
        drawPictures p (r:rows) = drawPictures (drawPic p r columnList) rows
            where
                drawPic :: Picture -> Int -> [Int] -> Picture
                drawPic pic _ []                 = pic
                drawPic pic row (column:columns) = drawPic newPic row columns
                    where
                        newPic = Pictures [pic, positionedSquare]
                        positionedSquare = 
                            drawSquare (round xCoord) (round yCoord) actualShape
                                where
                                    xCoord = left + fromIntegral column * squareWidth
                                    yCoord = bottom + fromIntegral row * squareHeight
                        actualShape = shapeAt board column row 

        drawCurPiece :: Picture -> [Int] -> Picture
        drawCurPiece p []     = p
        drawCurPiece p (b:bs) = drawCurPiece newP bs 
            where
                newP = Pictures [p, translate xCoord yCoord $ drawSquare (round xCoord) (round yCoord) actualShape]
                    where
                        curX = _curX board + (x (_curPiece board) b)
                        curY = _curY board + (y (_curPiece board) b)
                        xCoord = left + fromIntegral curX * squareWidth
                        yCoord = bottom + (boardHeight - fromIntegral curY - 1) * squareHeight
                        actualShape = _shape $ _curPiece board


timerEvent :: TetrixBoard -> TetrixBoard
timerEvent board = finalBoard
    where
        finalBoard = 
            if _isWaitingAfterLine board
                then newPiece (board { _isWaitingAfterLine = False }) { _timer = startTimer (_timer board) }
                else oneLineDown board


dropDown :: TetrixBoard -> TetrixBoard
dropDown board = finalBoard 
    where
        dropHeight = 0
        newY = _curY board

        incDropHeight :: TetrixBoard -> Int -> Int -> (TetrixBoard, Int)
        incDropHeight b 0 dh  = (b, dh)
        incDropHeight b ny dh = 
            if moved
                then incDropHeight board0 (ny - 1) (dh + 1)
                else incDropHeight board0 ny dh 
            where
                (board0, moved) = tryMove b (_curPiece board) (_curX board) (ny - 1)

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
        processDrop tb []     = tb
        processDrop tb (b:bs) = processDrop ntb bs
            where
                xCoord = _curX tb + (x (_curPiece tb) b)
                yCoord = _curY tb - (y (_curPiece tb) b)
                ntb = setShapeAt tb xCoord yCoord (_shape (_curPiece tb))

        board0 = processDrop board [0..3]

        board1 = board0 { _numPiecesDropped = _numPiecesDropped board + 1}

        board2 = 
            if _numPiecesDropped board1 `mod` 25 == 0
                then board2 {
                    _level = _level board2 + 1,
                    _timer = startTimer (_timer board2)
                    -- TODO emit level changed
                }
                else board2
        board3 = board2 {
            _score = _score board2 + dropHeight + 7
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
                    _score = _score b + 10 * nfl,
                    _timer = (_timer b) { _final = Just 500 },
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
                 _timer = stopTimer (_timer board3) ,
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
        centerSquare = 
            color (makeColorI r g b a) $
            translate centerX centerY $ 
            rectangleSolid (squareWidth - 2) (squareHeight - 2)

        leftLine = 
            color (makeColorI (r + 20) (g + 20) (b + 20) a) $
            line [
                (fromIntegral xCoord, fromIntegral yCoord + squareHeight - 1), 
                (fromIntegral xCoord, fromIntegral yCoord)
            ]  

        bottomLine = 
            color (makeColorI (r + 20) (g + 20) (b + 20) a) $
            line [
                (fromIntegral xCoord, fromIntegral yCoord),
                ((fromIntegral xCoord + squareWidth - 1), fromIntegral yCoord) 
            ]  

        topLine = 
            color (makeColorI (r - 20) (g - 20) (b - 20) a) $
            line [
                (fromIntegral xCoord + 1, fromIntegral yCoord + squareHeight - 1),
                (fromIntegral xCoord + squareWidth - 1, fromIntegral yCoord + squareHeight - 1) 
            ]  

        rightLine = 
            color (makeColorI (r - 20) (g - 20) (b - 20) a) $
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

