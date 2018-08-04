-- Functionally Solving Problems, problem 1

-- (Splitting up these files so that I can actually make the code compile for once)

-- Reverse Polish Notation solution is interesting, but I'm a little down on the mathematical system...

-- import Data.List

solveRPN :: (Num a, Read a) => String -> a  -- The result must be part of the "Read" type, since we call "Read" on the number
-- Would be a bit simplier to say "Float" than Num a and Read a
solveRPN = head . foldl pFold [] . words -- "words" is a function, not a variable! We're breaking our string into a series of numbers and symbols to read in succession
    where  pFold (x:y:ys) "*" = (x * y):ys -- Always operate on the two *most recent* additions to the stack (we go front-back, or "top-down")
           pFold (x:y:ys) "+" = (x + y):ys
           pFold (x:y:ys) "-" = (x - y):ys
           pFold ys numString = read numString:ys -- we build a stack as we go, then compress it when we hit operators (above)

-- adding extensions at this point is trivial. Woo!
-- We could also build in "Maybe Float" instead, for fault-tolerance, but let's wait for the Monad chapter...