-- Functionally Solving Problems, problem 1

-- (Splitting up these files so that I can actually make the code compile for once)

-- Reverse Polish Notation solution is interesting, but I'm a little down on the mathematical system...

-- import Data.List

solveRPN :: (Num a) => String -> a
solveRPN = head . foldl pFold [] . string 
    where  pFold (x:y:ys) "*" = (x * y):ys
           pFold (x:y:ys) "+" = (x + y):ys
           pFold (x:y:ys) "-" = (x - y):ys
           pFold ys numString = read numString:ys -- if we hit a number, turn it into a Num and add it to the stack