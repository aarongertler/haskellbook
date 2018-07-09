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


-- quicksort is faster with filtering

quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) = smaller ++ [x] ++ bigger
    where bigger = quicksort (filter (>x) xs)
          smaller = quicksort (filter (<=x) xs)


-- Finding the largest number under x, divisible by y

largestDivisible :: (Integral a) => a -> a -> a -- this works!
largestDivisible x y = head (filter p [x,(x-1)..])
    where p x = x `mod` y == 0 -- Need to create our predicate and include the fact that it acts on x


-- Putting together fancy results: et the sum of all odd squares under 10,000
-- Start by declaring the function result we are summing, then write what data we pass into the function
fancy = sum (takeWhile (<10000) (filter odd (map (^2) [1..])))
-- For numbers 1 and greater, map the square function to the list, only take odd values from that list, and keep adding values until we hit a value over 10,000
-- takeWhile gives us a list of eligible squares to sum


-- But I might prefer the list comprehension version:

fancyLC = sum (takeWhile (<10000) [n^2 | n <- [1..], odd (n^2)])
-- We are taking a list of numbers 1 and greater, passing in only the values of n
-- where n^2 is odd, and then adding the n^2 values to our takeWhile list
-- (using n^2 kind of stinks, but the above reads a bit more "sentence-like", so I'm unsure)


-- TEST: Build a function to generate Collatz sequences

collatz :: (Integral a) => a -> [a] -- Can't just return [a], you also need to specify that you take a
collatz 1 = [1] -- Remember that 1 is a part of the chain! No [] here
collatz n
    | even n = n : collatz (div n 2) -- need to chain n with the collatz, not [n] (else you produce a list of lists, not cool!)
    | odd n = n : collatz ((n * 3) + 1)


longChains = length (filter long (map collatz [1..100]))
    where long xs = length xs >= 15




-- You can map a set of Ords into functions like so:

functionList = map (++) ["Cat","Dog","Fish"] -- Prepare each string to have something added to it
result = (functionList !! 2) (" " ++ "Sticks") -- "Fish Sticks"


-- Lambdas = anonymous functions that get passed to higher-order functions

-- we can make our "longChains" function more efficient:

longChains = length (filter (\xs -> length xs > 15) (map collatz [1..100]))
-- We replace our where with an anonymous function, \xs

lambdaZip = zipWith (\a b -> (a * 12) / b) [1,2,3,4,5] [4,3,2,1,1]
-- Create an anonymous function that works with the values of both a and b,
-- so that we do something with both of the lists we are zipping togther

-- Lambdas can only match one pattern (no exceptions!)

-- Maybe the most readable version of flip:
flip' :: (a -> b -> c) -> b -> a -> c 
flip' f = \x y -> f y x -- We'll turn whatever f does into the opposite f
-- Good to use a lambda here, since we'll never actually feed parameters into our function f
-- (we just use it as a parameter)
-- #QUESTION: Why does the right association work in the "output" line of this function?

-- NOTE FOR THE ABOVE: (b -> a -> c) is the same as b -> a -> c
-- Type declaration just tells us we are returning three thing,
-- unnecessary to state that they are a function specifically
-- This still feels like a #QUESTION to me, not sure I understand the meaning of "right associative" well enough


-- Next up: foldl














-- showList = print functionList
-- MAKE THE ABOVE THING WORK!













-- REVIEW ALL THIS AS SOON AS YOU FINISH THE CHAPTER, IT IS IMPORTANT
