module Main (main) where
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game

windowDisplay :: Display
windowDisplay = InWindow "Tetrix" (400, 600) (800, 200)

data Piece = Piece { _piece :: Picture, posX :: Float, posY :: Float } 

data Board = Board { border :: [Picture], pieces :: [Piece]}

data GameState = GameState {
    _stepAcc :: Float,
    _board :: Board,
    _posY :: Float
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
    GameState {_stepAcc=0, _posY=0, _board = board}
    render
    handleEvent
    step

render :: GameState -> Picture
render state = translate 0 (_posY state - 10) $ rectangleSolid 50 50

handleEvent :: Event -> GameState -> GameState
handleEvent _ state = state

step :: Float -> GameState -> GameState 
step _ state = if _stepAcc state == 60 && _posY state > -280 
               then state { _stepAcc = 0, _posY = _posY state - 20 } 
               else state { _stepAcc = _stepAcc state + 1 }
