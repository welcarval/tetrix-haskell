module Main (main) where
import           Graphics.Gloss
import           Graphics.Gloss.Interface.Pure.Game
import           Graphics.Gloss.Juicy
import           System.Random                      (StdGen, newStdGen)
import           TetrixBoard
import           TetrixPiece

windowWidth :: Int
windowWidth = 900
windowHeight :: Int
windowHeight = 750

windowDisplay :: Display
windowDisplay = InWindow "Tetrix" (windowWidth, windowHeight) (800, 200)

data Assets = Assets {
    _gameOverAsset  :: Picture,
    _nextPieceAsset :: Picture,
    _levelAsset     :: Picture,
    _scoreAsset     :: Picture,
    _pausedAsset    :: Picture,
    _controlsAsset  :: Picture,
    _startGameAsset :: Picture
}

data RepeatableKey = RepeatLeft | RepeatRight | RepeatSoftDrop deriving Eq

toRepeatableKey :: Key -> Maybe RepeatableKey
toRepeatableKey (SpecialKey KeyLeft)  = Just RepeatLeft
toRepeatableKey (SpecialKey KeyRight) = Just RepeatRight
toRepeatableKey (Char 'd')            = Just RepeatSoftDrop
toRepeatableKey _                     = Nothing

fromRepeatableKey :: RepeatableKey -> Key
fromRepeatableKey RepeatLeft     = SpecialKey KeyLeft
fromRepeatableKey RepeatRight    = SpecialKey KeyRight
fromRepeatableKey RepeatSoftDrop = Char 'd'

keyRepeatInterval :: Float
keyRepeatInterval = 0.05

data TetrixWindow = TetrixWindow {
    _board                :: SomeTetrixBoard,
    _nextPieceL           :: Picture,
    _levelLabel           :: Picture,
    _scoreLabel           :: Picture,
    _controls             :: Picture,
    _scoreDisplay         :: Picture,
    _levelDisplay         :: Picture,
    _linesDisplay         :: Picture,
    _startButton          :: Picture,
    _quitButton           :: Picture,
    _pauseButton          :: Picture,
    _heldKey              :: Maybe RepeatableKey,
    _keyRepeatableElapsed :: Float,
    _stdGen               :: StdGen
}

createWindow :: StdGen -> Assets -> TetrixWindow
createWindow g assets =
    TetrixWindow {
        _board                = SomeTetrixBoard SCreated $ createBoard g boardLabels,
        _nextPieceL           = _nextPieceAsset assets,
        _levelLabel           = _levelAsset assets,
        _scoreLabel           = _scoreAsset assets,
        _controls             = _controlsAsset assets,
        _scoreDisplay         = text "0",
        _levelDisplay         = text "0",
        _linesDisplay         = text "0",
        _startButton          = text "start",
        _quitButton           = text "quit",
        _pauseButton          = text "pause",
        _heldKey              = Nothing,
        _keyRepeatableElapsed = 0,
        _stdGen               = g
    }
    where boardLabels = BoardLabels {
            _overlayStartGame = _startGameAsset assets,
            _overlayPaused    = _pausedAsset assets,
            _overlayGameOver  = _gameOverAsset assets
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
handlerEvent e@(EventKey key Down _ _) window =
    window {
        _board = keyPressEvent e (_board window),
        _heldKey = case toRepeatableKey key of
            Just rk -> Just rk
            Nothing -> _heldKey window,
        _keyRepeatableElapsed = case toRepeatableKey key of
            Just _  -> 0
            Nothing -> _keyRepeatableElapsed window
    }
handlerEvent (EventKey key Up _ _) window =
    window {
        _heldKey = if toRepeatableKey key == _heldKey window
            then Nothing
            else _heldKey window
    }
handlerEvent _ window = window


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

    let assets = Assets {
            _gameOverAsset  = gameOverPng,
            _nextPieceAsset = nextPiecePng,
            _levelAsset     = levelPng,
            _scoreAsset     = scorePng,
            _pausedAsset    = pausedPng,
            _controlsAsset  = controlsPng,
            _startGameAsset = startGamePng
        }
        window = createWindow g assets

    play
        windowDisplay
        black
        60
        window
        paintWindow
        handlerEvent
        step

step :: Float -> TetrixWindow -> TetrixWindow
step dt window = windowAfterTimer { _keyRepeatableElapsed = newElapsed, _board = newBoard }
    where
        windowAfterTimer = window { _board = advanceTimer (_board window) }
        elapsed = _keyRepeatableElapsed windowAfterTimer + dt

        (newElapsed, newBoard) =
            case _heldKey windowAfterTimer of
                Just rk | elapsed >= keyRepeatInterval -> (0, keyPressEvent (repeatEvent rk) (_board windowAfterTimer))
                _                                      -> (elapsed, _board windowAfterTimer)
        repeatEvent rk = EventKey (fromRepeatableKey rk) Down (Modifiers Up Up Up) (0, 0)
