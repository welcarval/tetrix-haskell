module Main (main) where
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game

windowDisplay :: Display
windowDisplay = InWindow "Tetrix" (400, 600) (800, 200)

data Piece = Piece { _piece :: Picture, posX :: Float, posY :: Float } 

data Board = Board { border :: [Picture], pieces :: [Piece]}

data GameState = GameState {
    stepAcc :: Float,
    _board :: Board
}

board :: Board
board = Board {border = [color white (line [(0, 0), (0, 100)]), 
                        color white (line [(0, 100), (100, 100)]), 
                        color white (line [(100, 100), (100, 0)]), 
                        color white (line [(100, 0), (0, 0)])
                        ], 
               pieces =  [Piece { _piece = color (makeColorI 10 178 10 200) $ rectangleSolid 20 20, posX = 0, posY = 0 }]
              }


main :: IO ()
main = play
    windowDisplay
    black
    60
    GameState {stepAcc=0, posY=0, _board = board}
    render
    handleEvent
    step

render :: GameState -> Picture
render state = translate 0 (posY state - 10) $ board

handleEvent :: Event -> GameState -> GameState
handleEvent _ state = state

step :: Float -> GameState -> GameState 
step _ state = if stepAcc state == 60 && posY state > -280 
               then state { stepAcc = 0, posY = posY state - 20 } 
               else state { stepAcc = stepAcc state + 1 }
