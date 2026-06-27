module Main (main) where
import Graphics.Gloss
import TetrixBoard 
import TetrixPiece
import System.Random (newStdGen, StdGen)
import Graphics.Gloss.Interface.Pure.Game

windowDisplay :: Display
windowDisplay = InWindow "Tetrix" (900, 750) (800, 200)

data TetrixWindow = TetrixWindow {
    _board :: TetrixBoard,
    _nextPieceL :: Picture,
    _scoreDisplay :: Picture,
    _levelDisplay :: Picture,
    _linesDisplay :: Picture,
    _startButton :: Picture,
    _quitButton :: Picture,
    _pauseButton :: Picture,
    _stdGen :: StdGen
}

createWindow :: StdGen -> TetrixWindow
createWindow g = TetrixWindow {
    _board = createBoard g,
    _nextPieceL = text "NextPiece",
    _scoreDisplay = text "0",
    _levelDisplay = text "0",
    _linesDisplay = text "0",
    _startButton = text "start",
    _quitButton = text "quit",
    _pauseButton = text "pause",
    _stdGen = g
}

paintWindow :: TetrixWindow -> Picture
paintWindow window = finalPicture
    where
        leftSide = Pictures [leftSideTitle, drawNextPieceLabel]
        centerSide = paintEvent board
        rightSide = blank

        board = _board window

        finalPicture = Pictures[leftSide, centerSide, rightSide]

        leftSideTitle = translate (-410) (250) $ scale 0.3 0.3 $ color white $ text "NEXT PIECE"

        nextPieceLabelX = -320
        nextPieceLabelY = 140
        nextPieceLabelShape = _shape (_nextPiece board)

        squareCordsShape = coordsTable !! fromEnum nextPieceLabelShape

        drawNextPieceLabel = drawPic [0..3] 
            where
                drawPic :: [Int] -> Picture
                drawPic []                 = blank
                drawPic (square:squares)   = Pictures [positionedSquare, drawPic squares]
                    where
                        positionedSquare = 
                            drawSquare (round xCoord) (round yCoord) nextPieceLabelShape
                                where
                                    squareCoords = squareCordsShape !! square
                                    xCoord = nextPieceLabelX + fromIntegral (squareCoords !! 0) * squareWidth
                                    yCoord = nextPieceLabelY + fromIntegral (squareCoords !! 1) * squareHeight

handlerEvent :: Event -> TetrixWindow -> TetrixWindow
handlerEvent e window = finalWindow
    where 
        board = _board window 
        finalWindow = window { _board = (keyPressEvent e board) }


main :: IO ()
main = do
    g <- newStdGen
    let window = createWindow g
    play
        windowDisplay
        black
        60
        window
        paintWindow
        handlerEvent
        step

step :: Float -> TetrixWindow -> TetrixWindow 
step _ window = finalWindow
    where
        board = _board window
        
        isPaused = _isPaused board
        
        timer0 = _timer board 
        timer1 = if isPaused then timer0 else timer0 { _actual = (_actual timer0) + 1}

        board0 = board { _timer = timer1 }
        finalBoard = 
            if (_actual timer1) >= (_final timer1)
                then board2
                else
                    board0
                where
                    timer2 = timer1 { _actual = 0 }
                    board1 = board { _timer = timer2 }
                    board2 = timerEvent board1

        finalWindow = window { _board = finalBoard }



    
