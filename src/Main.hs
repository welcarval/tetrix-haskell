module Main (main) where
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Juicy
import System.Random (StdGen, newStdGen)
import TetrixBoard
import TetrixPiece

windowWidth :: Int
windowWidth = 900
windowHeight :: Int
windowHeight = 750

windowDisplay :: Display
windowDisplay = InWindow "Tetrix" (windowWidth, windowHeight) (800, 200)

data TetrixWindow = TetrixWindow {
    _board        :: SomeTetrixBoard,
    _nextPieceL   :: Picture,
    _levelLabel   :: Picture,
    _scoreLabel   :: Picture,
    _controls     :: Picture,
    _scoreDisplay :: Picture,
    _levelDisplay :: Picture,
    _linesDisplay :: Picture,
    _startButton  :: Picture,
    _quitButton   :: Picture,
    _pauseButton  :: Picture,
    _stdGen       :: StdGen
}

createWindow :: StdGen -> Picture -> Picture -> Picture -> Picture -> Picture -> Picture -> Picture -> TetrixWindow
createWindow g gameOverLabel nextPieceL levelLabel scoreLabel pausedLabel controls startGameLabel =
    TetrixWindow {
        _board = SomeTetrixBoard SCreated $ createBoard g gameOverLabel pausedLabel startGameLabel,
        _nextPieceL = nextPieceL,
        _levelLabel = levelLabel,
        _scoreLabel = scoreLabel,
        _controls = controls,
        _scoreDisplay = text "0",
        _levelDisplay = text "0",
        _linesDisplay = text "0",
        _startButton = text "start",
        _quitButton = text "quit",
        _pauseButton = text "pause",
        _stdGen = g
}

paintWindow :: TetrixWindow -> Picture
paintWindow window = paintBoard window (_board window)

paintBoard :: TetrixWindow -> SomeTetrixBoard -> Picture
paintBoard window someBoard@(SomeTetrixBoard _ board) = finalPicture
    where
        leftSide = Pictures [nextPieceBlock, levelBlock, scoreBlock]
        centerSide = paintEvent someBoard
        rightSide = translate sideWidth 0 $ scale 0.6 0.6 $ (_controls window)

        sideWidth = fromIntegral windowWidth / 3

        leftSideBlockHeight = fromIntegral windowHeight / 3

        letterWidth = (75 :: Double) * 0.3

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

        nextPiece = _nextPiece board
        drawNextPieceLabel = Pictures [drawSquare (x nextPiece i * round squareWidth)
                                         (y nextPiece i * round squareHeight)
                                         nextPieceLabelShape | i <- [0..3]]

handlerEvent :: Event -> TetrixWindow -> TetrixWindow
handlerEvent e window = finalWindow
    where
        board = _board window
        finalWindow = window { _board = (keyPressEvent e board) }


main :: IO ()
main = do
    g <- newStdGen
    Just gameOverPng  <- loadJuicyPNG "assets/gameover.png"
    Just nextPiecePng <- loadJuicyPNG "assets/nextpiece.png"
    Just levelPng     <- loadJuicyPNG "assets/level.png"
    Just scorePng     <- loadJuicyPNG "assets/score.png"
    Just pausedPng    <- loadJuicyPNG "assets/paused.png"
    Just controlsPng  <- loadJuicyPNG "assets/controls.png"
    Just startGamePng <- loadJuicyPNG "assets/startgame.png"

    let window =
            createWindow
                g
                gameOverPng
                nextPiecePng
                levelPng
                scorePng
                pausedPng
                controlsPng
                startGamePng
    play
        windowDisplay
        black
        60
        window
        paintWindow
        handlerEvent
        step

step :: Float -> TetrixWindow -> TetrixWindow
step _ window = window { _board = advanceTimer (_board window)}
