-- Higher-order functions in Haskell

-- All Haskell functions only take "one parameter"
-- So things like "max" are "curried functions"

a = max 4 5

-- For the above, we create a function "max 4" that looks at
-- y and returns the bigger of 4 or y
-- Then we apply the "max 4" function to our y-value (5)

-- max :: (Ord a) => a -> (a -> a) -- One way to think of max:
-- It takes an Ord, then produces a function which takes an Ord and produces an Ord

mult :: (Num a) => a -> a -> a
mult x y = x * y

multByThree = mult 3

b = multByThree 7 -- 21