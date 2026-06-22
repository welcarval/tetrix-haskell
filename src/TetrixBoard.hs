module TetrixBoard () where

import TetrixPiece
import Graphics.Gloss

boardWidth :: Float
boardWidth = 10

boardHeight :: Float
boardHeight = 22

data TetrixBoard = TetrixBoard {
    -- TODO this is actually a Qtimer, to handle timer Events, see what fits better later
    _timer :: Int,
    _nextPieceLabel :: Maybe String,
    _isWaitingAfterLine :: Bool,
    _curPiece :: TetrixPiece,
    _nextPiece :: TetrixPiece,
    _curX :: Int,
    _curY :: Int,
    _numLinesRemoved :: Int,
    _numPiecesDropped :: Int,
    _score :: Int,
    _level :: Int,
    _board :: Maybe [Shape],
    -- equivalent to setFrameStyle
    _frameStyle :: Picture,
    -- TODO see whats equivalent to focusPolicy, maybe related do EventHandling
    _isStarted :: Bool,
    _isPaused :: Bool
}

createBoard gen = TetrixBoard {
    _timer = 0, 
    _nextPieceLabel = Nothing, 
    _isWaitingAfterLine = False, 
    _curPiece = createPiece, 
    _nextPiece = piece, 
    _curX = 0, 
    _curY = 0, 
    _numLinesRemoved = 0, 
    _numPiecesDropped = 0, 
    _score = 0, 
    _level = 0, 
    _board = Just [NoShape | _ <- [0..(boardWidth * boardHeight)]], 
    _frameStyle = rectangleSolid boardWidth boardHeight, 
    _isStarted = False, 
    _isPaused = False
    }
    where
    (piece, _) = setRandomShape createPiece gen
