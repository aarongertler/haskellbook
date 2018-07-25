-- Input and output in Haskell

-- Reminder: We only ever return a new thing, we never modify an old thing (example: How tree inserts work in Haskell vs. Python)

-- main = putStrLn "Hello, world!" -- takes a string, returns an I/O action with a return value of () -- that is, no meaningful return (the empty tuple)

-- ghc --make filename, and then ./filename

-- I/O actions are performed when we name them "main" and run them as a program

main = do
  putStrLn "Hello, what's your name?"
  name <- getLine -- not an equals sign, be careful!   -- this is an I/O action with a result -- we open a blank line and return a string
  putStrLn ("Hey, " ++ name ++ "!") -- must be in parens to work as I/O output

-- getLine is never a variable on its own -- because I/O code is "impure" (can have different results when run twice), we store it in a safe variable
-- getLine has a type of "IO String", not "String"