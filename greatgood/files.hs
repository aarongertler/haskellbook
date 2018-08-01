-- Files and streams in Haskell

import Data.Char
import Data.List
import System.Environment
import System.Directory
import System.IO
import System.Random

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
	    numberedTasks = zipWith (\n line -> show n ++ " - " ++ line) [0..] todoTasks -- each line is zipped to a number: ["8 - Buy Bananas", "9 - Smell the Roses", etc.]
	putStrLn "These are your TO-DO items:"
	putStr $ unlines numberedTasks -- Return the full, numbered list
	putStrLn "Which one do you want to delete?"
	numberString <- getLine -- We need our answer entered as a single number, like "8", not "08" or "eight"
	let number = read numberString
	    newTodoItems = delete (todoTasks !! number) todoTasks -- todoTasks is still split into lines, and not numbered
			-- reminder: "delete" returns a new list without our item -- it doesn't modify an existing list
	hPutStr tempHandle $ unlines newTodoItems -- turn our set of remaining todos back into one big string, and connect that string to our new file through the handle
	hClose handle
	hClose tempHandle -- everything is connected where it needs to be, so we can close the handles -- we're done changing files
	removeFile "todo.txt" -- close handle before removing
	renameFile tempName "todo.txt" -- now that we don't have todo.txt anymore, we can re-establish it with our new, slimmer list
      -- removeFile and renameFile take paths, not just strings, as their parameters (but we are in the current directory)


-- Let's take some command-line arguments!

main = do
  args <- getArgs -- makes a list of the arguments the program was run with      
  progName <- getProgName -- returns the program's name. Neat!
  putStrLn "The arguments are:"
  mapM putStrLn args -- we use mapM for I/O monad purposes (everything is printed in one I/O action)
  putStrLn "The program name is:"
  putStrLn progName


-- Now to integrate the todo app with the command line:

dispatch :: [(String, [String] -> IO ())] -- takes a list of k/v pairs (strings and I/O functions, respectively)
dispatch = [ ("add", add)
           , ("view", view)
           , ("remove", remove)
           ]

main = do
	(command:args) <- getArgs -- bind first argument as "command", the rest as a list of "args"
	let (Just action) = lookup command dispatch -- "lookup" finds the right value in our "dispatch" k/v dictionary (so if we wrote "add" in the command line, we'd find the add function)
	  -- We want a Maybe here in case someone doesn't include a command
	action args -- then we run the appropriate function on our list of args

add :: [String] -> IO () -- get a list with a filename and todo item, turn into an appendFile (I/O action)
add [fileName, todoItem] = appendFile fileName (todoItem ++ "\n") -- mustn't forget the newline!

view :: [String] -> IO ()
view [fileName] = do
	contents <- readFile fileName -- set up the handle
	let todoTasks = lines contents
	    numberedTasks = zipWith (\n line -> show n ++ " - " ++ line) [0..] todoTasks -- see other zipWith, above
	putStr $ unlines numberedTasks

remove :: [String] -> IO ()
remove [fileName, numberString] = do
	handle <- openFile fileName ReadMode -- we need fancy file creation/deletion privileges, because every deletion means a new file
	(tempName, tempHandle) <- openTempFile "." "temp"
	contents <- hGetContents handle
	let number = read numberString
	    todoTasks = lines contents
	    newTodoItems = delete (todoTasks !! number) todoTasks
	hPutStr tempHandle $ unlines newTodoItems
	hClose handle
	hClose tempHandle
	removeFile fileName
	renameFile tempName fileName

-- Next improvement to this would probably be graceful failure in the form of an "error" I/O action or exception



-- Randomness! (Has its own typeclasses for purity reasons, since "normal" typeclasses should always return the same output given the same input)

coinToss :: StdGen -> Bool
coinToss gen = random newGen -- we always need to return a generator alongside our random value (#QUESTION: Is this proper newGen generation? Does this work?)
-- When we call this, it would look like: threeCoins (mkStdGen X), where X was a number, and every new number would flip a different random coin

-- Creating limitless randomness (currently available through "randoms")
randoms' :: (RandomGen g, Random a) => g -> [a] -- Take a generator, produce a random value of a chosen type
randoms' gen = let (value, NewGen) = random gen in value:randoms' newGen -- produce an infinite list, where each new random value is added sequentially, with a new generator paving the way for the next random value
-- Could easily be made non-infinite, too (see the book's "finiteRandoms")

-- randomR produces a random value from within a range; randomRs does this multiple times


-- Why is this I/O-related? Well, we don't want to keep seeding our own generators. getStdGen lets us pull randomness from our system, as with other languages

main = do
	gen <- getStdGen
	putStr $ take 20 (RandomRs ('a','z') gen) -- Produce a 20-character random string and set our new generator as whatever the system gave us (so the next string will be just as random)
  -- But don't run getStdGen twice in one "main", or you'll get the same string twice (newStdGen fixes this by producing an extra generator)
	-- REMEMBER: The gen is not the random number, it's just the machine we need to pass into any function that creates randomness (that's why it's the second parameter of RandomRs)


	-- You left off on Control.Monad