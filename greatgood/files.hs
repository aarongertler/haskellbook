-- Files and streams in Haskell

import Data.Char

main = do
	contents <- getContents -- until we force-exit the program, take in whatever text we input (one line, if we're at the terminal)... 
	putStr (shortLinesOnly contents) -- ...and return it caps-locked
	-- We can grab a whole file's worth of test this way
	-- Lazyness means that we only actually run "getContents" when we hit putStr and need something to return

	shortLinesOnly :: String -> String
	shortLinesOnly input =
		let allLines = lines input -- break down the input by newline character
		    shortLines = filter (\line -> length line < 10) allLines -- for each line, include only if length < 10
		    result = unlines shortLines -- reverse the breakdown, turn all the lines into one string
		in result -- we define what we return with the "in" line

-- The pattern above is very common (get a string from I/O action, run it through a function, output the result)

-- And we can make it even shorter with "interact", which combines "contents <- getContents" and "putStr":
-- main = interact shortLinesOnly

-- Or we can one-line it!
-- main = interact $ unlines . filter ((<10) . length) . lines    -- no need to specify a variable, since we've used function composition

-- "interact" can give back one result, or keep taking input forever -- depends on how content is entered (terminal vs. file)


-- openFile :: FilePath -> IOMode --> IO Handle   
-- = take a filepath and IOMode (ReadMode, WriteMode, AppendMode, ReadWriteMode), output an IO Handle (action that opens the file and produces a result)