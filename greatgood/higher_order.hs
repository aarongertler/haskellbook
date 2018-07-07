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

-- applyTwice (++ " HAHA") "HEY"  
-- "HEY HAHA HAHA"  
-- applyTwice ("HAHA " ++) "HEY" -- The operand goes where it is needed
-- "HAHA HAHA HEY"  

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c] -- Takes a function and two lists, returns a third list
zipWith' _ [] _ = []
zipWith' _ _ [] = [] -- Function is always in front, only zip as much stuff as the shorter of the two lists holds
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys -- Apply the function to the front item of each list, then repeat for as long as the shortest list still has items

z = zipWith' (*) [3,4,5] [2,3,4] -- [6,12,20]

zz = zipWith' (zipWith' (*)) [[1,2,3],[4,5,6]] [[1.5,2.5,3.5],[1.5,2.5,3.5]] 
-- This lets us combine lists within lists: First we zipWith' the first two lists, then the second two lists


-- Building functions to transform other functions

flip' :: (a -> b -> c) -> (b -> a -> c) -- Works for any two-parameter function, returns another two-parameter function
flip' f x y = f y x -- Create a function that works the same as f, but with the parameters reversed
-- Why this works: We output a function, f, and define the value of that function in "where"

divide = flip (/)
flipdiv = divide 2 4 -- 2 
flipzip = flip' zip [1,2,3] ['a','b','c'] -- [('a',1),('b',2),('c',3)]


-- map is list comprehension, but MUCH MORE READABLE
addthree = map (+3) [1,2,3,4]
badthree = [x+3 | x <- [1,2,3,4]]


-- TEST: Write "filter" from scratch
filter' :: (a -> Bool) -> [a] -> [a] -- Input true/false test and list, return list
filter' _ [] = []
filter' p (x:xs) -- need p, not _, since we actually need to refer to this function later
    | p x = x : filter' p xs
    | otherwise = filter' p xs -- only attach x to the new list if it meets our condition

filterthree = filter' (>3) [1,2,3,4,5]
badfilterthree = [(x) | x <- [1,2,3,4,5], x>3] -- Not awful, still less readable    	










-- REVIEW ALL THIS AS SOON AS YOU FINISH THE CHAPTER, IT IS IMPORTANT
