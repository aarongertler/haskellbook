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
-- could also be: multByThree x = mult 3 x

b = multByThree 7 -- 21


compareWithFive :: (Num a, Ord a) => a -> Ordering -- Need our input to be in both the Ord and Num typeclasses (else we can't compare to 5)
compareWithFive = compare 5                        -- "compare" by itself would just be (Ord a)


divideByFive :: (Floating a) => a -> a -- Take and return a float/double
divideByFive = (/5) -- Infix function with a section
-- Sections create functions where the parameter is applied wherever an operand is missing (in this case, to the "back" of the number we enter). Neat!


applyTwice :: (a -> a) -> a -> a -- take a function (that returns the same type of thing we put in) and a parameter, return a parameter
applyTwice f x = f (f x) -- Whoa!