module Main where 
import MancalaBoard
import MancalaAI
main :: IO ()

main = do 
	putStrLn "Welcome to Mancala!"
	putStrLn "Here's how to play. Type in a number from 0-5  indicating which pit you want to take pebbles from and distribute. Your pit is tagged with the number 6.The game ends when one of the players have no pebbles left on his side to distribute. At this point, the player with the most number of pebbles on his side is the winner!" 
	putStrLn "To quit at any time, type CTRL C."
	g <- promptForString "Do you want to start? (yes/no)" 
	if g == "yes"
		then playGame initial 
			else endGame initial 
playGame :: MancalaBoard -> IO ()
playGame initial = do 
	putStrLn "\n"
	putStrLn "Okay, let us start."
	putStrLn "\n"
	putStrLn "Player A, begin."
	putStrLn "This is your current board"
	print initial 
	chooseMove initial

nextTurn :: MancalaBoard -> IO()
nextTurn a = do
	putStrLn "\n"
	putStrLn "It is your turn."
	putStrLn "This is the current board:"
	print a 
	chooseMove a


chooseMove :: MancalaBoard -> IO() 
chooseMove a = do 
	putStrLn "Here are your allowed moves:" 
	print $ allowedMoves a 
	g <- promptForString "Which pit do you select?" 
	doMove a (read g :: Int) 

computerPlayerMove :: MancalaBoard -> IO()
computerPlayerMove a = do 
	t <- return $ aiNextMove a 
	d <- return $ move a t
	isItGameOver d
 
doMove :: MancalaBoard -> Int -> IO ()
doMove a g = do
	if isAllowedMove a g 
		then do 
		     t <- return $ move a g
		     isItGameOver t 
		else do
			putStrLn "That is not a valid move. Choose another pit!"
			chooseMove a 


isItGameOver :: MancalaBoard -> IO ()
isItGameOver t = do
 	if gameOver t
		then do
			putStrLn "We're finished!" 
			putStrLn "The winner is..."
			print $ winners t
			playAgain
		else do
			m <- return $ getCurPlayer t 
			if (show m) == "PlayerB"
				then do 
					computerPlayerMove t 
				else do 
					nextTurn t
				 
playAgain :: IO ()
playAgain = do
	t <- promptForString "Want to play again? (yes/no)"
	if t == "yes" 
		then playGame initial
		else endGame initial

endGame :: MancalaBoard -> IO ()
endGame initial = do 
	putStrLn "Thanks for playing. Goodbye."

promptForString :: String -> IO String
promptForString s = do
	putStrLn s
	getLine

