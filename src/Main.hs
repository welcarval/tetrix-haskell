module Main (main) where
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import TetrixBoard (TetrixBoard, createBoard, paintEvent)
import System.Random (newStdGen)

windowDisplay :: Display
windowDisplay = InWindow "Tetrix" (400, 760) (800, 200)

main :: IO ()
main = do
    g <- newStdGen
    let board = createBoard g
    play
        windowDisplay
        black
        60
        board
        paintEvent
        handleEvent
        step

-- render :: GameState -> Picture
-- render state = translate 0 (_posY state - 10) $ rectangleSolid 50 50

handleEvent :: Event -> TetrixBoard -> TetrixBoard
handleEvent _ state = state

step :: Float -> TetrixBoard -> TetrixBoard 
step _ board  = board
-- step _ state = if _stepAcc state == 60 && _posY state > -280 
--                then state { _stepAcc = 0, _posY = _posY state - 20 } 
--                else state { _stepAcc = _stepAcc state + 1 }
