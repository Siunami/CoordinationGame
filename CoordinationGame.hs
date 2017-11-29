-- run exec from CoordinationGame/dist/build/CoordinationGame/CoordinationGame
module CoordinationGame (defaultMain) where

import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort
import Graphics.Gloss.Data.Picture
import Graphics.Gloss.Data.Color
import Graphics.Gloss.Data.Bitmap

-- window properties
width, height, offset :: Int
width = 1500
height = 1000
offset = 100

window :: Display
window = InWindow "Coordination Game" (width, height) (offset, offset)

background :: Color
background = white



-- data structure to hold state of game
-- player1 turn to select rows
-- player2 turn to select column
-- score of player1
-- score of player2
-- board loaded
data CoordinationGame = Game
	{	player1 :: [Integer] -- player1's coordinates chosen
	,	player2 :: [Integer] -- player2's coordinates chosen
	,	scoreP1 :: Integer -- score of player1
	,	scoreP2 :: Integer -- score of player2
	,	board :: [[Integer]]
	} deriving Show


-- initialize game (starting state for coordination game)
initialState :: CoordinationGame
initialState = Game
	{	player1 = [0,0]
	,	player2 = [0,0]
	,	scoreP1 = 0
	,	scoreP2 = 0
	,	board = [[0,0],[0,0],[0,0],[0,0]]
	}


-- draw game state (convert it to picture)
render :: CoordinationGame -> Picture
render game =
	pictures [box, lineX, lineY, 
							topLeft, topRight, 
							bottomLeft, bottomRight,
							p1Score,
							p2Score] -- add player# game here
									 -- player# selections
	where
		-- table
		box = rectangleWire 1000 600
		lineY = line [(0,-300),(0,300)]
		lineX = line [(-500,0),(500,0)]

		p1Score = translate (-500) 350 $ text ("Player 1 Score: " ++ (show 0)) 
		p2Score = translate (-500) (-450) $ text ("Player 2 Score: " ++ (show 0)) 

		-- topLeft returns the board coordinates and renders to top-left position
		topLeft = translate (-375) 100 $ text (show (selectTopLeft (board game) 1))
		
		-- topRight returns the board coordinates and renders to top-right position
		topRight = translate 125 100 $ text (show (selectTopRight (board game) 1))
		
		-- bottomLeft returns the board coordinates and renders to bottom-left position
		bottomLeft = translate (-375) (-200) $ text (show (selectBottomLeft (board game) 1))
		
		-- bottomRight returns the board coordinates and renders to bottom-right position
		bottomRight = translate 125 (-200) $ text (show (selectBottomRight (board game) 1))


--simulate :: Display -> Color -> Int -> initState -> (initState -> Picture) -> (ViewPort -> Float -> initState -> initState) -> IO()

-- | Number of frames to show per second.
fps :: Int
fps = 30

updateGame :: Float -> CoordinationGame -> CoordinationGame
updateGame seconds game = game { board = [[1,1],[1,1],[1,1],[1,1]] }

-- | Update the game when player2 makes a selection.
update :: ViewPort -> Float -> CoordinationGame -> CoordinationGame
update _ = updateGame

-- rendering world state
-- number of games with boards
defaultMain :: IO()
defaultMain = simulate window background fps initialState render update












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

-- boards
board1 = [[1,2],[3,4],[5,6],[7,8]]

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






