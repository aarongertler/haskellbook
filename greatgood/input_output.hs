-- Input and output in Haskell

-- Reminder: We only ever return a new thing, we never modify an old thing (example: How tree inserts work in Haskell vs. Python)

-- main = putStrLn "Hello, world!" -- takes a string, returns an I/O action with a return value of () -- that is, no meaningful return (the empty tuple)

-- ghc --make filename, and then ./filename

-- I/O actions are performed when we name them "main" and run them as a program

import Data.Char

-- main = do
--   putStrLn "Hello, what's your first name?"
--   first <- getLine -- not an equals sign, be careful!   -- this is an I/O action with a result -- we open a blank line and return a string
--   putStrLn "What's your last name?"
--   last <- getLine
--   let bigFirst = map toUpper first  -- let bindings work for non-I/O actions
--       bigLast = map toUpper last
--   putStrLn ("Hey, " ++ bigFirst ++ " " ++ bigLast ++ "!") -- must be in parens to work as I/O output

-- getLine is never a variable on its own -- because I/O code is "impure" (can have different results when run twice), we store it in a safe variable
-- getLine has a type of "IO String", not "String"

main = do
	line <- getLine
	if null line
		then return () -- an "empty" I/O action, not an end to the program (if it weren't in an if/then, we could keep going after the return)
		else do 
			putStrLn (reverseWords line)
			main -- Now we have an input loop! We'll keep asking for words until the user hits "enter" with no word, at which point we return nothing
			-- This whole "do" (putting a line and returning to main) counts as one I/O action

reverseWords :: String -> String
reverseWords = unwords . map reverse . words -- "Words" turns a string into a space-separated list of words, "unwords" turns a list into a single string
-- Dot placement = function composition = "words" happens to the line first, then "map reverse", then "unwords"
-- Without it, we'd need to explicitly state the variable with something like:
-- reverseWords st = unwords (map reverse (words st))

-- think of function composition as being equivalent to nested parens: "f of g of h"
-- but it spares us the need to pump in the variable
-- trying reverseWords st = unwords . map reverse (words st) won't work, because we expect to get a function parameter for map reverse (a0 -> [String])
-- but instead get a [[Char]] (the result of words st)
-- seems like you can't go halfway with function composition?
-- Definition of composition: (f . g) x = f (g x)
-- So the above broken code is trying to return a function call rather than a string? That's my best interpretation... #QUESTION
-- While the correct code knows it will be running all the functions on a string, and thus will get the right output? ...yeah, #QUESTION


-- OTHER NOTES ON INPUT/OUTPUT:

-- putStr is like putStrLn without a line break (think of Ruby, puts vs. put)
-- putChar outputs one character at a time (and is recurred to build putStr)
-- print = putStrLn . show  (turns anything that can be a string into a string with "show", then prints it) (ghci uses this for terminal work!)


-- Every program that takes user input will only operate once we hit "enter" (even if we tell it to stop accepting new data after we enter a space --
	-- if that happens, we'll just get the first word of whatever we wrote)


-- "when" is Haskell's way of saying "as long as..." 
-- (replaces "if something do I/O else return ()" with "when something do I/O", handles the "return on False" automatically)


-- "sequence" lets you line up a list of I/O actions:

-- main = do  
--     a <- getLine  
--     b <- getLine  
--     c <- getLine  
--     print [a,b,c]  

-- ...is the same as:

-- main = do  
--     rs <- sequence [getLine, getLine, getLine]  
--     print rs  


-- use mapM_ when you want to print a series of items from a mapped list, but don't want a list of empty returns to print out at the end


-- main = forever $ do (stuff)   -- this will cause some console thing to happen forever, with no return condition (is this a common occurrence?)


-- forM = "make an I/O action for every element in this list" 
-- (example: Printing a set of quiz questions, saving each answer, then feeding all answers back to the user) (see book example)
-- Helps with readability, doesn't do anything you couldn't replace with other functions



-- Author's guide to thinking through this stuff:

-- Don't think of a function like putStrLn as a function that takes a string and prints it to the screen. 
-- Think of it as a function that takes a string and returns an I/O action. 
-- That I/O action will, when performed, print beautiful poetry to your terminal.