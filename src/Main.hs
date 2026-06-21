module Main (main) where
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game

windowDisplay :: Display
windowDisplay = InWindow "Tetrix" (400, 600) (800, 200)

data GameState = GameState {
    stepAcc :: Float,
    posY :: Float
}

main :: IO ()
main = play
    windowDisplay
    black
    60
    GameState {stepAcc=0, posY=0}
    render
    handleEvent
    step

render :: GameState -> Picture
render state = translate 0 (posY state - 10) $ color (makeColorI 10 178 10 200) $ rectangleSolid 20 20

handleEvent :: Event -> GameState -> GameState
handleEvent _ state = state

step :: Float -> GameState -> GameState 
step _ state = if stepAcc state == 60 && posY state > -280 
               then state {
                          stepAcc = 0, 
                          posY = posY state - 20
                          } 
               else state {
                          stepAcc = stepAcc state + 1
                          }
