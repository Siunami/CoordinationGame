module CoordinationGame (defaultMain) where

import Graphics.Gloss (play, Display(InWindow))
import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort
import Graphics.Gloss.Data.Picture
import Graphics.Gloss.Data.Color
import Graphics.Gloss.Data.Bitmap
import Graphics.Gloss.Interface.Pure.Game

-- | Window properties
width, height, offset :: Int
width = 1500
height = 1000
offset = 100

-- | Window background color
background :: Color
background = white

-- | Creates window
window :: Display
window = InWindow "Coordination Game" (width, height) (offset, offset)


--board1 = []
-- data structure to hold state of game
-- player1 turn to select rows
-- player2 turn to select column
-- score of player1
-- score of player2
-- board loaded
data CoordinationGame = Game
	--{	player1 :: [Integer] -- player1's coordinates chosen
	--,	player2 :: [Integer] -- player2's coordinates chosen
	{	scoreP1 :: Integer -- score of player1
	,	scoreP2 :: Integer -- score of player2
	,	board :: [[Integer]]
	,	p1Select :: Bool
	,	p2Select :: Bool
	,	p1Selection :: Integer
	,	p2Selection :: Integer
	,	lob :: [[[Integer]]]
	} deriving Show

board1 = [[4,4],[3,1],[1,3],[2,2]]
board2 = [[3,3],[5,0],[0,5],[0,0]]
board3 = [[5,5],[6,4],[4,6],[4,4]]
board4 = [[2,2],[2,0],[0,2],[0,0]]
board5 = [[3,5],[2,2],[2,2],[5,3]]
board6 = [[2,7],[5,0],[0,5],[7,2]]
boardList = [board1, board2, board3, board4, board5, board6]

-- initialize game (starting state for coordination game)
initialState :: CoordinationGame
initialState = Game
	{	--player1 = [0,0]
	--,	player2 = [0,0]
		scoreP1 = 0
	,	scoreP2 = 0
	,	p1Select = False
	,	p2Select = False
	,	p1Selection = 0
	,	p2Selection = 0
	-- Selection is 1 or 2, 0 for unchosen
	,	board = board1
	,	lob = boardList
	}


-- draw game state (convert it to picture)
render :: CoordinationGame -> Picture
render game =
	pictures [box, lineX, lineY, 
							topLeft, topRight, 
							bottomLeft, bottomRight,
							p1Score,
							p2Score]

	where
		-- table
		box = rectangleWire 1000 600
		lineY = line [(0,-300),(0,300)]
		lineX = line [(-500,0),(500,0)]

		p1Score = translate (-500) 350 $ text ("Player 1 Score: " ++ (show (scoreP1 game))) 
		p2Score = translate (-500) (-450) $ text ("Player 2 Score: " ++ (show (scoreP2 game))) 

		-- topLeft returns the board coordinates and renders to top-left position
		topLeft = translate (-375) 100 $ text (show (selectTopLeft (board game) 1))
		topRight = translate 125 100 $ text (show (selectTopRight (board game) 1))
		bottomLeft = translate (-375) (-200) $ text (show (selectBottomLeft (board game) 1))
		bottomRight = translate 125 (-200) $ text (show (selectBottomRight (board game) 1))

		--p1Selection = if p1Selects1 game then show (selectP1 game) else blank


-- rendering world state
-- number of games with boards
defaultMain :: IO()
defaultMain = play window background 5 initialState render handleInput step


--getGrid :: Boolean -> Boolean -> Integer

--getGrid True True = 1
--getGrid True False = 2
--getGrid False True = 3
--getGrid False False = 4

--calculateScoreP1 :: [[Integer]] -> Boolean -> Boolean -> Integer
--calculateScoreP1 b p1 p2 = 

-- | Player selects keys 
handleInput :: Event -> CoordinationGame -> CoordinationGame

handleInput (EventKey (Char 'a') Down _ _) game =
	--if not(p1Select game)
	--	then
	--		game { p1Select = True, scoreP1 = 1 }
	--	else if not(p2Select game) && p1Select game
	--		then
	--			game { p2Select = True, scoreP2 = 5 }
	--		else if p2Select game && p1Select game
	--			then
	--				game { board = board2, p2Select = False, p1Select = False, scoreP1 = 2 }
	--			else 
	--				game { board = board2, scoreP2 = 3 }
	if p2Select game && p1Select game
		then
			game { board = 
				board2, 
				p2Selection = 0,
				p1Selection = 0, 
				p2Select = False, 
				p1Select = False, 
				scoreP1 = 2 
			}
		else if not(p2Select game) && p1Select game
			then
				game { p2Select = True, p2Selection = 1, scoreP2 = 0 }
			else if not(p1Select game)
				then
					game { p1Select = True, p1Selection = 1, scoreP1 = 0 }
				else 
					game


--game { board = board2 }

--handleInput (EventKey (Char 'd') _ _ _) game =
--	if notp1Select game
--		then do
--			game { notp1Select = False, p1Selection = 2, scoreP1 = 1 }
--		else if notp2Select game && not(notp1Select game)
--			then
--				game { notp2Select = False, p2Selection = 2, scoreP2 = 1 }
--			else if not(notp2Select game) && not(notp1Select game)
--				then
--				game { board = board2, notp2Select = False, notp1Select = False, scoreP1 = 2 }

handleInput _ game = game


----  | Player 1 selects key 1
--handleInput (EventKey (Char '1') _ _ _) game =
--	game { selectP1 = take 2 board }

---- | Player 1 selects key 2
--handleInput (EventKey (Char '2') _ _ _) game =
--  	game { selectP1 = drop 2 board }

-- | Player 2 selects key 3
--handleInput (EventKey (Char '3') _ _ _) game =

-- | Player 2 selects key 4
--handleInput (EventKey (Char '4') _ _ _) game =


-- Do nothing for all other events.
--handleInput _ game = game


step :: Float -> CoordinationGame -> CoordinationGame
step _ w = w

--nashEquilibrium :: [[Integer]] -> Integer -> Float
--nashEquilibrium board 

-- num equals 1 initially
selectFirst :: [Integer] -> Integer -> Integer
selectFirst (a:b) 1 = a
selectFirst (a:b) num = selectFirst b (num + 1)

selectSecond :: [Integer] -> Integer -> Integer
selectSecond (a:b) 2 = a
selectSecond (a:b) num = selectSecond b (num + 1)




-- IO basic examples
-- how to print things in haskell
io1 = putStrLn "Hello World!"

-- printing multiple lines
io2 = do
    putStrLn "The answer is: "
    print 43

io3 = do
    print "The answer is: "
    print 43

-- recieving user input
io4 = do
    str <- getLine
    putStrLn str


checkYes "y" = do
	print "yes detected. Run function here to start game"
checkYes "n" = do
	print "no detected. Return to main menu"
	main
checkYes "q" = do
	print "q detected. closing...."
checkYes a = do
	print "Sorry I don't understand"
	main

-- Main allows a user to start a game
main = do
	print "Start game y/n, q to quit"
	str <- getLine
	checkYes str


-- test for interacting with lists
list1 = [1,2,3,4,5]
list2 = addArray list1

addArray :: [Integer] -> Integer
addArray [] = 0
addArray (a:b) = a + addArray(b)
---

-- method to add board values using addArray helper defined above
nestedArrayAddition :: [[Integer]] -> Integer
nestedArrayAddition [] = 0
nestedArrayAddition (a:b) = addArray(a) + nestedArrayAddition(b)


-- select functions take a board and the integer starting pos 1
selectTopLeft :: [[Integer]] -> Integer -> [Integer]
selectTopLeft (a:b) 1 = a

selectTopRight :: [[Integer]] -> Integer -> [Integer]
selectTopRight (a:b) 2 = a
selectTopRight (a:b) num = selectTopRight b (num + 1)

selectBottomLeft :: [[Integer]] -> Integer -> [Integer]
selectBottomLeft (a:b) 3 = a
selectBottomLeft (a:b) num = selectBottomLeft b (num + 1)

selectBottomRight :: [[Integer]] -> Integer -> [Integer]
selectBottomRight (a:b) 4 = a
selectBottomRight (a:b) num = selectBottomRight b (num + 1)







