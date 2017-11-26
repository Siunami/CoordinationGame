module CoordinationGame where


-- tests for interacting with lists
list1 = [1,2,3,4,5]
list2 = addArray list1

addArray :: [Integer] -> Integer
addArray [] = 0
addArray (a:b) = a + addArray(b)
---


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


-- boards
board1 = [[1,2],[3,4],[5,6],[7,8]]

-- method to add board values using addArray helper defined above
nestedArrayAddition :: [[Integer]] -> Integer
nestedArrayAddition [] = 0
nestedArrayAddition (a:b) = addArray(a) + nestedArrayAddition(b)








