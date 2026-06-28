module Main (main) where
import Graphics.Gloss
import Graphics.Gloss.Juicy
import TetrixBoard 
import TetrixPiece
import System.Random (newStdGen, StdGen)
import Graphics.Gloss.Interface.Pure.Game

windowWidth = 900
windowHeight = 750

windowDisplay :: Display
windowDisplay = InWindow "Tetrix" (windowWidth, windowHeight) (800, 200)

data TetrixWindow = TetrixWindow {
    _board :: TetrixBoard,
    _nextPieceL :: Picture,
    _levelLabel :: Picture,
    _scoreLabel :: Picture,
    _scoreDisplay :: Picture,
    _levelDisplay :: Picture,
    _linesDisplay :: Picture,
    _startButton :: Picture,
    _quitButton :: Picture,
    _pauseButton :: Picture,
    _stdGen :: StdGen
}

createWindow :: StdGen -> Picture -> Picture -> Picture -> Picture -> Picture -> TetrixWindow
createWindow g gameOverLabel nextPieceL levelLabel scoreLabel pausedLabel = TetrixWindow {
    _board = start (createBoard g gameOverLabel pausedLabel),
    _nextPieceL = nextPieceL,
    _levelLabel = levelLabel,
    _scoreLabel = scoreLabel,
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
        leftSide = Pictures [nextPieceBlock, levelBlock, scoreBlock]
        centerSide = paintEvent board
        rightSide = blank

        sideWidth = fromIntegral windowWidth / 3
        sideWidthCenter = sideWidth / 2
         
        leftSideWidth = sideWidth
        centerSideWidth = sideWidth
        rightSideWidth = sideWidth

        sideHeight = windowHeight
        sideHeightCenter = fromIntegral windowHeight / 2

        leftSideBlockHeight = fromIntegral windowHeight / 3
        leftSideBlockWidth = leftSideWidth
        leftSideBlockGap = 10
        leftSideBlockMargin = 20


        letterWidth = (75 :: Double) * 0.3
        wordSize = length "NEXT PIECE" 
        wordCenter = fromIntegral wordSize * letterWidth / 2

        board = _board window

        finalPicture = Pictures[leftSide, centerSide, rightSide]

        nextPieceBlock = translate (-sideWidth) (leftSideBlockHeight) $ Pictures[nextPieceTitle, nextPieceDraw]
        nextPieceTitle = translate (0) (50) $ _nextPieceL window
        nextPieceDraw = translate (-squareWidth / 2) (-50) $ scale 0.7 0.7 $ drawNextPieceLabel

        levelBlock = translate (-sideWidth) (0) $ Pictures [levelTitle, levelValue] 
        levelTitle = translate (0) (40) $ _levelLabel window
        levelValue = translate (calculateWordCenter (show (_level board))) (-40) $ scale 0.3 0.3 $ color white $ text (show (_level board))

        scoreBlock = translate (-sideWidth) (-leftSideBlockHeight) $ Pictures [scoreTitle, scoreValue]
        scoreTitle = translate 0 40 $ _scoreLabel window
        scoreValue = translate (calculateWordCenter (show (_score board))) (-40) $ scale 0.3 0.3 $ color white $ text (show (_score board))

        calculateWordCenter word = realToFrac (-(fromIntegral (length word) * letterWidth / 2))

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
                                    xCoord = fromIntegral (squareCoords !! 0) * squareWidth
                                    yCoord = fromIntegral (squareCoords !! 1) * squareHeight

handlerEvent :: Event -> TetrixWindow -> TetrixWindow
handlerEvent e window = finalWindow
    where 
        board = _board window 
        finalWindow = window { _board = (keyPressEvent2 e board) }


main :: IO ()
main = do
    g <- newStdGen
    Just gameOverPng  <- loadJuicyPNG "assets/gameover.png"
    Just nextPiecePng <- loadJuicyPNG "assets/nextpiece.png"
    Just levelPng     <- loadJuicyPNG "assets/level.png"
    Just scorePng     <- loadJuicyPNG "assets/score.png"
    Just pausedPng    <- loadJuicyPNG "assets/paused.png"
    let window = 
            createWindow 
                g 
                gameOverPng 
                nextPiecePng 
                levelPng 
                scorePng 
                pausedPng
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
        isStarted = _isStarted board
        state = _state board
        
        timer0 = _timer board 
        timer1 = if state /= Running then timer0 else timer0 { _actual = (_actual timer0) + 1}

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



    
