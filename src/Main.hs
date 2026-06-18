module Main (main) where
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Simulate

windowDisplay :: Display
windowDisplay = InWindow "Tetrix" (200, 200) (500, 200)

type Model = (Float, Float)

main :: IO ()
main = simulate 
    windowDisplay 
    white 
    simulationRate
    initialModel
    drawingFunc
    updateFunc
    where
        simulationRate :: Int
        simulationRate = 20

        initialModel :: Model
        initialModel = (0, 0)

        drawingFunc :: Model -> Picture
        drawingFunc (theta, _) = Line [(0, 0), (50 * cos theta, 50 * sin theta)]

        updateFunc :: ViewPort -> Float -> Model -> Model
        updateFunc _ dt (theta, dtheta) = (theta + dt * dtheta, dtheta - dt * (cos theta))
