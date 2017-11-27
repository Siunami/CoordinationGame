-- run exec from CoordinationGame/dist/build/CoordinationGame/CoordinationGame
module CoordinationGame (defaultMain) where

import Graphics.Gloss

-- window properties
width, height, offset :: Int
width = 800
height = 500
offset = 100

window :: Display
window = InWindow "Coordination Game" (width, height) (offset, offset)

background :: Color
background = white

-- drawing takes in a board and renders the world state
drawing :: [[Integer]] -> Picture
drawing board = pictures [box, lineX, lineY, topLeft, topRight, bottomLeft, bottomRight]
	where
		-- table
		box = rectangleWire 500 300
		lineX = line [(0,-150),(0,150)]
		lineY = line [(-250,0),(250,0)]

		-- topLeft returns the board coordinates and renders to top-left position
		topLeft = translate (-250) 30 $ text (show (selectTopLeft board 1))
		
		-- topRight returns the board coordinates and renders to top-right position
		topRight = translate 20 30 $ text (show (selectTopRight board 1))
		
		-- bottomLeft returns the board coordinates and renders to bottom-left position
		bottomLeft = translate (-250) (-130) $ text (show (selectBottomLeft board 1))
		
		-- bottomRight returns the board coordinates and renders to bottom-right position
		bottomRight = translate 20 (-130) $ text (show (selectBottomRight board 1))


-- rendering the window
defaultMain :: IO()
defaultMain = display window background (drawing board1)


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






