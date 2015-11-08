module MancalaBoard (MancalaBoard, Player(PlayerA, PlayerB), initial, 
            getCurPlayer, playerNum, playerWithNum, nextPlayer,
            getBoardData, numCaptured, move, allowedMoves, playerSide,
            isAllowedMove, gameOver, winners, indexForPlayerStore,
            playerScore) where

import List -- for List.elemIndex
import Maybe -- for List.elemIndex

{-
 - The stones on a Mancala board are simply recorded as a list of Ints.  The
 -  Ints come in the following order:
 - 1. The boardSize pits belonging to PlayerA
 - 2. The store belonging to PlayerA
 - 3. The boardSize pits belonging to PlayerB
 - 4. The store belonging to PlayerB
 -}

data MancalaBoard = MancalaBoardImpl [Int] Player

data Player = PlayerA | PlayerB deriving (Eq, Show)

---- Functions/constants for Player ----

allPlayers = [PlayerA, PlayerB]
numPlayers = length allPlayers


playerNum :: Player -> Int
playerNum p = fromJust $ List.elemIndex p allPlayers


playerWithNum :: Int -> Player
playerWithNum i = allPlayers !! i


nextPlayer :: Player -> Player
{- Find the player whose turn is next -}
nextPlayer p = playerWithNum $ ((playerNum p) + 1) `mod` numPlayers


---- Functions/constants for MancalaBoard ----

{- number of pits on each side -}
boardSize = 6
{- number of stones in each pit -}
startStones = 4

{- the initial mancala board -}
initial :: MancalaBoard
initial = MancalaBoardImpl (concat $ take numPlayers (repeat boardSide)) PlayerA
                        -- One side of board                pit at end
    where boardSide = take boardSize (repeat startStones) ++ [0]


{- return the index of the first pit belonging to a player -}
indexForFirstPit :: Player -> Int
indexForFirstPit p = (playerNum p) * (boardSize + 1)


{- return the index of the store for that player -}
indexForPlayerStore :: Player -> Int
indexForPlayerStore p = boardSize + (indexForFirstPit p)


{- return the indices for the pits (without the store) for a player -}
indicesForPlayerSide :: Player -> [Int]
indicesForPlayerSide p = [firstPit .. lastPit] where
    firstPit = indexForFirstPit p
    lastPit = firstPit + boardSize - 1


---- Retrieve information about Mancala Board
-- TODO: uncomment these type declarations and implement the functions
{- return the player who has the current turn -}
getCurPlayer :: MancalaBoard -> Player
getCurPlayer (MancalaBoardImpl _ p) = p


{- return the list of all pits in the board -}
getBoardData :: MancalaBoard -> [Int]
getBoardData (MancalaBoardImpl boardData _) = boardData


{- return the side of the board for a specified player, including the store at
 - the end -}
playerSide :: MancalaBoard -> Player -> [Int]
playerSide (MancalaBoardImpl boardData _) player = 
            take (boardSize + 1) $ drop (indexForFirstPit player) boardData


{- return the number of captured pieces in specified player's store -}
numCaptured (MancalaBoardImpl boardData _) player = boardData !! (indexForPlayerStore player)

playerScore :: MancalaBoard -> Player -> Int
playerScore b p = sum $ playerSide b p

{- allowedMoves returns a list of valid moves for the current player:
 - ie. the indices of pits which belong to that player, and which contain one
 - or more pieces -}
allowedMoves :: MancalaBoard -> [Int]
allowedMoves mancala = allowedMovesFor mancala (getCurPlayer mancala)

allowedMovesFor :: MancalaBoard -> Player -> [Int]
allowedMovesFor (MancalaBoardImpl boardData _) player = [i | i <- indicesForPlayerSide player, (boardData !! i) /= 0]

{- check that a move is valid for the current player -}
isAllowedMove :: MancalaBoard -> Int -> Bool
isAllowedMove b i = i `elem` (allowedMoves b)



{- gameOver checks to see if the game is over (i.e. if one player's side of the
 - board is all empty -}
gameOver :: MancalaBoard -> Bool
-- Assume two players
-- The game is over if the last player ended his turn with an empty side
--      (it is sufficient to check that either side is empty, 
--        as the game must have ended this turn or the previous turn
--        if either side is empty)
gameOver b = (sum $ take boardSize $ playerSide b PlayerA) == 0 ||
             (sum $ take boardSize $ playerSide b PlayerB) == 0


{- winner returns a list of players who have the top score: there will only be 
 - one in the list if there is a clear winner, and none if it is a draw -}
winners :: MancalaBoard -> [Player]
-- assume two players, and that game is over
winners b
    | ascore > bscore = [PlayerA]
    | bscore > ascore = [PlayerB]
    | otherwise = [PlayerA, PlayerB] where 
          ascore = sum $ playerSide b PlayerA
          bscore = sum $ playerSide b PlayerB


---- show
instance Show MancalaBoard where
    show (MancalaBoardImpl boardData player) =
            (show boardData) ++ " " ++ (show player)



{- We number the pits from 0 to 13 (2 players, 6 pits each and 1 store each)
 - This function takes a board and applies the move where the player selects
 - the numbered pit, giving back an updated board after the move -}

{- One approach: build a function that tells you how many pieces
 - added to each else -}
move :: MancalaBoard -> Int -> MancalaBoard
move board i 
   | i >= 0 && i <= 13 = MancalaBoardImpl updatedBoard nextPlayer' 
   | otherwise = error "move index out of range [0,13]"
   where
        (before, (n:after)) = splitAt i $ getBoardData board
        -- remove the seeds from the specified pit
        seedsRemoved = before ++ (0:after)
        -- Number of times it totally goes around board
        fullLaps = n `div` (totalBoardSize - 1)
        leftover = n `mod` (totalBoardSize - 1)
        
        -- Helper calculations
        totalBoardSize = (boardSize + 1) * numPlayers
        playerStore = indexForPlayerStore $ getCurPlayer board
        -- Assume two players
        opponentStore = indexForPlayerStore $ nextPlayer $ getCurPlayer board

        -- how many steps around board from selected pit are we, skipping
        --  the opponent's store.  This function assumes j < boardSize
        stepsAway j
            | relPos < oppStoreRelPos = relPos
            | otherwise = relPos - 1 where 
            relPos = (j - i) `mod` totalBoardSize
            oppStoreRelPos = (opponentStore - i) `mod` totalBoardSize

        nextPlayer'
            | stepsAway playerStore == leftover = getCurPlayer board
            | otherwise = nextPlayer $ getCurPlayer board

        -- Number of pieces to add to position j
        addedPieces j
            | j == opponentStore = 0
            | stepsAway j > 0 && stepsAway j <= leftover = fullLaps + 1
            | otherwise = fullLaps
        
        -- update the board with the added pieces 
        updatedBoard = [pieces + (addedPieces i) | (i, pieces) 
                            <- zip [0..(length seedsRemoved)] seedsRemoved] 

