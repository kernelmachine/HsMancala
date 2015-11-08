module MancalaAI(aiNextMove) where

import MancalaBoard
import Data.List
import Data.Maybe

type Move = Int

-- Decides what move to make for the current player of the specified MancalaBoard.
aiNextMove :: MancalaBoard ->  Move
aiNextMove a = (allowedMoves a) !! (maxIndex $ lookahead a 1)

maxIndex :: [Int] -> Int
maxIndex xs = head $ filter ((== maximum xs) . (xs !!)) [0..]

heuristicScore :: MancalaBoard -> Player -> Int
heuristicScore a p = sum (playerSide a p) - sum (playerSide a (nextPlayer p))

evalPosition :: MancalaBoard -> Int -> Int
evalPosition a 0 = heuristicScore a (getCurPlayer a)

evalPosition a depth = last ((heuristicScore a (getCurPlayer a)) : [evalPosition (move a ((allowedMoves a) !! (maxIndex $ lookahead a (depth-1)))) (depth-1)]) 
lookahead :: MancalaBoard -> Int -> [Move]
lookahead mancala depth = do
	a <- allowedMoves mancala
	b <- [move mancala a]
	if gameOver b == True
		then do
			c <- [a]
			return c 
		else do
			c <- [evalPosition b depth]
			return c
