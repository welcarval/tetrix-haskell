module Main (main) where
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game

windowDisplay :: Display
windowDisplay = InWindow "Tetrix" (400, 600) (800, 200)

type World = Float

main :: IO ()
main = play
    windowDisplay
    black
    60
    0.0
    render
    handleEvent
    step

render :: World -> Picture
render world = translate 0 (world - 10) $ color (makeColorI 10 178 10 200) $ rectangleSolid 20 20

handleEvent :: Event -> World -> World
handleEvent _ world = world

step :: Float -> World -> World
step dt world = if dt `mod` 60.0 == 0 && world > -280 then world - 20 else world
