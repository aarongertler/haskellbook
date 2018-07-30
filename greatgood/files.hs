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



import System.IO  
  
-- main = do  
--     handle <- openFile "test.txt" ReadMode  
--     contents <- hGetContents handle   -- Add the text of whatever we just read from the file we opened (takes in a handle, gives back an I/O string)
																				 -- hGetContents can take any handle, getContents just reads from std input (terminal)
																				 -- it doesn't get loaded in memory for future use -- we only look at it once we need it *right now* (thanks, laziness!)
--     putStr contents  
--     hClose handle    -- always close the files you've opened!

-- openFile :: FilePath -> IOMode --> IO Handle   
-- = take a filepath and IOMode (ReadMode, WriteMode, AppendMode, ReadWriteMode)
-- output an IO Handle (action that opens the file and produces a result, the "handle")

-- If contents = chapter of a book, handle = bookmark to show us what chapter we're on (lets us "open" and read the chapter, or scribble on it


-- withFile takes a FilePath, IOMode, and function (Handle -> IO a), then returns an I/O action that will do something with the file before closing it

main = do
	withFile "test.txt" ReadMode (\handle -> do -- This is the function we wrote for taking a handle and returning an I/O action
		contents <- hGetContents handle
		putStr contents)
-- End result: putStr the contents of the file, but also close it afterwards 

withFile' :: FilePath -> IOMode -> (Handle -> IO a) -> IO a
withFile' path mode f = do -- this is how we start up an I/O action, remember!
    handle <- openFile path mode
    result <- f handle -- the handle tells us where to find our "chapter" and what we can do with it -- the function we run on it needs something like hGetContents to actually *do* stuff
    hClose handle -- the bonus feature of withFile -- closing things
    return result 


-- other h functions (hPutStrLn, hGetChar, etc.) let us read and write to files with more control


-- Shortcuts for the fancy withFile stuff above (shortcuts within shortcuts!)

main = do  
    contents <- readFile "test.txt" -- just grabs the string we wanted, no need to manually add handle management
    putStr contents  

main = do     
    contents <- readFile "test.txt"     
    writeFile "testcaps.txt" (map toUpper contents) -- Make a new file that we've run through a function

main = do     
    todoItem <- getLine  
    appendFile "todo.txt" (todoItem ++ "\n")  -- like writeFile, but works for adding to existing files AND creating new files


-- (Chapter includes some information about buffering that doesn't seem relevant for now)


-- And now, for our biggest program ever! To be written line-by-line

main = do
	handle <- openFile "todo.txt" ReadMode -- We're only reading the file, since we can't "write" a deletion (instead, we'll use the Haskell-y technique of deleting and replacing with a new file)
	(tempName, tempHandle) <- openTempFile "." "temp" -- creating a temporary file in our directory (".") with a name that will be "tempXXX", where "XXX" is random stuff
	contents <- hGetContents handle -- create a variable to hold the full text of todo.txt in a string, using the handle
	let todoTasks = lines contents -- split our full text into lines (each line is a task)
	    numberedTasks = zipWith (\n line -> show n ++ " - " ++ line) [0..] todoTasks -- each line is zipped to a number: "8 - Buy Bananas"
	putStrLn "These are your TO-DO items:"
	putStr $ unlines numberedTasks -- Return the full, numbered list
	putStrLn "Which one do you want to delete?"
	numberString <- getLine -- We need our answer entered as a single number, like "8", not "08" or "eight"
	let number = read numberString
	    newTodoItems = delete (todoTasks !! number) todoTasks -- todoTasks is still split into lines, and not numbered
	hPutStr tempHandle $ unlines newTodoItems -- turn our set of remaining todos back into one big string, and connect that string to our new file through the handle
	hClose handle
	hClose tempHandle -- everything is connected where it needs to be, so we can close the handles -- we're done changing files
	removeFile "todo.txt" -- close handle before removing
	renameFile tempName "todo.txt" -- now that we don't have todo.txt anymore, we can re-establish it with our new, slimmer list
      