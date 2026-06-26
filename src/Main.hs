module Main (main) where
import Graphics.Gloss
import TetrixBoard 
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
        (start board)
        paintEvent
        keyPressEvent
        step

step :: Float -> TetrixBoard -> TetrixBoard 
step _ board = finalBoard
    where
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



    
