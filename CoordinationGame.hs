module CoordinationGame (defaultMain) where

import Graphics.Gloss (play, Display(InWindow))
import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort
import Graphics.Gloss.Data.Picture
import Graphics.Gloss.Data.Color
import Graphics.Gloss.Data.Bitmap
import Graphics.Gloss.Interface.Pure.Game
import Data.Maybe
import Control.Exception

-- | Window properties
width, height, offset :: Int
width = 1800
height = 1500
offset = 100

-- | Window background color
background :: Color
background = white

-- | Creates window
window :: Display
window = InWindow "Coordination Game" (width, height) (offset, offset)


-- | Game data structure
data CoordinationGame = Game
	{	scoreP1 :: Integer 			-- score of player1
	,	scoreP2 :: Integer 			-- score of player2
	,	board :: [[Integer]]		-- current board
	, 	selectP1 :: [[Integer]]
	,	selectP2 :: [[Integer]]
	, 	boardList :: [[[Integer]]]	-- list of boards
	,	p1Select :: Bool
	,	p2Select :: Bool
	,	p1Selection :: Integer
	,	p2Selection :: Integer
	,	turnP1 :: Bool 				-- player1 turn
	,	turnP2 :: Bool
	} deriving Show

board1 = [[4,4],[3,1],[1,3],[2,2]]
board2 = [[3,3],[5,0],[0,5],[0,0]]
board3 = [[5,5],[6,4],[4,6],[4,4]]
board4 = [[2,2],[2,0],[0,2],[0,0]]
board5 = [[3,5],[2,2],[2,2],[5,3]]
board6 = [[2,7],[5,0],[0,5],[7,2]]


-- | Initialize game (starting state for coordination game)
initialState :: CoordinationGame
initialState = Game
	{	scoreP1 = 0
	,	scoreP2 = 0
	,	board = board1
	,	selectP1 = [[]]
	,	selectP2 = [[]]
	,	boardList = [board2, board3, board4, board5, board6]
	,	p1Select = False
	,	p2Select = False
	,	p1Selection = 0
	,	p2Selection = 0
	,	turnP1 = True
	}


-- | Draw game state (convert it to picture)
renderStart :: CoordinationGame -> Picture
renderStart game =
	pictures [box, lineX, lineY, 
							topLeft, topRight, 
							bottomLeft, bottomRight,
							p1Score,
							p2Score,
							showTurn,
							p1Agree,
							p1Disagree,
							p2Agree,
							p2Disagree]

	where

		p1Agree = (translate (-600) (100) $ text ("A"))
		p1Disagree = (translate (-600) (-200) $ text ("D"))
		p2Agree = (translate (-300) (320) $ text ("A"))
		p2Disagree = (translate (200) (320) $ text ("D"))

		showTurn = if turnP1 game == True && not(p2Select game) || not(p1Select game)
						then rotate (-90) (translate (-500) (700) $ text ("Player 1 Turn"))
							else if turnP1 game == False && not(p2Select game) || not(p1Select game) 
								then
									translate (-500) (500) $ text ("Player 2 Turn")
								else
									rotate (-90) (translate (-500) (700) $ text ("Press anything for next round"))

		-- matrix table
		box = rectangleWire 1000 600
		lineY = line [(0,-300),(0,300)]
		lineX = line [(-500,0),(500,0)]

		p1Score = translate (-500) (-500) $ text ("Player 1 Score: " ++ (show (scoreP1 game))) 
		p2Score = translate (-500) (-650) $ text ("Player 2 Score: " ++ (show (scoreP2 game))) 

		-- topLeft returns the board coordinates and renders to top-left position
		topLeft = translate (-375) 100 $ text (show (selectTopLeft (board game) 1))
		topRight = translate 125 100 $ text (show (selectTopRight (board game) 1))
		bottomLeft = translate (-375) (-200) $ text (show (selectBottomLeft (board game) 1))
		bottomRight = translate 125 (-200) $ text (show (selectBottomRight (board game) 1))

-- | Draws end game state
renderEnd :: CoordinationGame -> Picture
renderEnd game =
	pictures [p1Score, p2Score, showScore]

	where
		p1Score = translate (-500) 350 $ text ("Player 1 Score: " ++ (show (scoreP1 game))) 
		p2Score = translate (-500) (-350) $ text ("Player 2 Score: " ++ (show (scoreP2 game))) 
		showScore = if (scoreP1 game > scoreP2 game) then translate (-500) 0 $ text ("Player 1 Wins") else translate (-500) 0 $ text ("Player 2 Wins")

-- | Draw game state based on current board
chooseRender :: CoordinationGame -> Picture
chooseRender game =
	if (board game == [])
		then renderEnd game
		else renderStart game




-- | Renders world state
defaultMain :: IO()
defaultMain = play window background 5 initialState chooseRender handleInput step
	




getGrid :: Integer -> Integer -> Integer
getGrid 1 1 = 1
getGrid 1 2 = 2
getGrid 2 1 = 3
getGrid 2 2 = 4

calculateScoreP1 :: [[Integer]] -> Integer -> Integer
calculateScoreP1 b 1 = b!!0!!0
calculateScoreP1 b 2 = b!!1!!0
calculateScoreP1 b 3 = b!!2!!0
calculateScoreP1 b 4 = b!!3!!0

calculateScoreP2 :: [[Integer]] -> Integer -> Integer
calculateScoreP2 b 1 = b!!0!!1
calculateScoreP2 b 2 = b!!1!!1
calculateScoreP2 b 3 = b!!2!!1
calculateScoreP2 b 4 = b!!3!!1


-- | Player selects keys 
handleInput :: Event -> CoordinationGame -> CoordinationGame


-- | Player 1 selects key 1 selects row 1

handleInput (EventKey (Char 'a') Down _ _) game =
	if p2Select game && p1Select game
		then
			game { 
				scoreP1 = scoreP1 game + calculateScoreP1 (board game) (getGrid (p1Selection game) (p2Selection game)),
				scoreP2 = scoreP2 game + calculateScoreP2 (board game) (getGrid (p1Selection game) (p2Selection game)),
				board = case listToMaybe (boardList game) of
								Nothing -> []
								Just first -> first
				,
				boardList = drop 1 (boardList game),
				p2Selection = 0,
				p1Selection = 0, 
				p2Select = False, 
				p1Select = False
			}
			else
				if not(p2Select game) && p1Select game
					then
						game { p2Select = True, turnP1 = True, p2Selection = 1 }
						else
							if not(p1Select game)
								then
									game { p1Select = True, turnP1 = False, p1Selection = 1 }
									else 
										game

--game { board = board2 }

handleInput (EventKey (Char 'd') Down _ _) game =
	if p2Select game && p1Select game
		then
			game { 
				scoreP1 = scoreP1 game + calculateScoreP1 (board game) (getGrid (p1Selection game) (p2Selection game)),
				scoreP2 = scoreP2 game + calculateScoreP2 (board game) (getGrid (p1Selection game) (p2Selection game)),
				board = case listToMaybe (boardList game) of
								Nothing -> []
								Just first -> first
				,
				boardList = drop 1 (boardList game),
				p2Selection = 0,
				p1Selection = 0, 
				p2Select = False, 
				p1Select = False
			}
			else
				if not(p2Select game) && p1Select game
					then
						game { p2Select = True, turnP1 = True, p2Selection = 2 }
						else
							if not(p1Select game)
								then
									game { p1Select = True, turnP1 = False, p1Selection = 2 }
									else 
										game

-- do nothing if other keys are pressed
handleInput _ game = game

step :: Float -> CoordinationGame -> CoordinationGame
step _ w = w

step :: Float -> CoordinationGame -> CoordinationGame
step _ w = w


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







