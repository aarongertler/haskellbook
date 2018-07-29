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

-- longChains = length (filter (\xs -> length xs > 15) (map collatz [1..100]))
-- We replace our where with an anonymous function, \xs

lambdaZip = zipWith (\a b -> (a * 12) / b) [1,2,3,4,5] [4,3,2,1,1]
-- Create an anonymous function that works with the values of both a and b,
-- so that we do something with both of the lists we are zipping togther

-- Lambdas can only match one pattern (no exceptions!)

-- Maybe the most readable version of flip:
-- flip' :: (a -> b -> c) -> b -> a -> c 
-- flip' f = \x y -> f y x -- We'll turn whatever f does into the opposite f
-- Good to use a lambda here, since we'll never actually feed parameters into our function f
-- (we just use it as a parameter)
-- #QUESTION: Why does the right association work in the "output" line of this function?

-- NOTE FOR THE ABOVE: (b -> a -> c) is the same as b -> a -> c
-- Type declaration just tells us we are returning three thing,
-- unnecessary to state that they are a function specifically
-- This still feels like a #QUESTION to me, not sure I understand the meaning of "right associative" well enough


-- Next up: folding!
-- Take each item in a list and do something with it, which may or may not depend on the other elements in the list
-- This is a lot like a for loop in other languages... maybe?

-- sum' :: (Num a) => [a] -> a
-- sum' = foldl (\acc x -> acc + x) 0
-- At each stage, acc increases by x; it starts at 0
-- No need to write "sum' xs", because currying lets us assume we'll pass in a list

-- Even faster:
sum' :: (Num a) => [a] -> a
sum' = foldl (+) 0 -- If we're performing a series of two-parameter functions on our accumulator and the next x, this style works


elem' :: (Eq a) => a -> [a] -> Bool
elem' x = foldl (\acc y -> if y == x then True else acc) False
-- Starting with the value "false" for acc, we check each element in the list
-- and switch to "True" permanently if we find what we're looking for

-- Note that \acc y is a lambda function, which we r


-- How foldr works:
-- foldr :: (a -> b -> b) -> b -> [a] -> b
-- foldr f acc []     = acc
-- foldr f acc (x:xs) = f x (foldr f acc xs)

map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\x acc -> f x : acc) []
-- Fold implies that we have a list already, and will move through the list
-- We move through our list, prepending each result so that we rebuild the mapped list from nothing
-- If we used foldl here, we'd need to ++ to build our list,
-- which is much slower than using :, so it's a great opportunity for foldr

-- So for map (*2) onto [1,2,3], we get:
-- f x (foldr f acc xs) = (*2) 1 : (foldr f acc [2,3])
-- and then (*2) 1 : (*2) 2 : (foldr f acc [3])
-- ... and so on. And "acc" represents the rest of the foldr stuff because foldr
-- generates a function where the second parameter is the "acc" 
-- foldr (all the other stuff) is just another way of saying "acc"
-- (until you get to the end and combine the original acc with the other things you've accumulated)

-- Explanation for why x and acc need to be in the "same order" above:
-- https://en.wikibooks.org/wiki/Haskell/Lists_III
-- foldl f acc (x:xs) =  foldl f (f acc x) xs    versus:
-- foldr f acc (x:xs) = f x (foldr f acc xs)
-- It's the way we order the parameters of the function, that's all
-- (and it should be clear why foldr moves from R-L, and foldl from L-R)


-- Lots of example folds in this chapter. My favorites:

reverse' :: [a] -> [a]
reverse' = foldl (\acc x -> x : acc) [] -- build from back to font, simple

maximum' :: (Ord a) => [a] -> a
maximum' = foldl1 (\acc x -> if x > acc then x else acc)
-- We use "foldl1" for maximum', which automatically uses the first element as our starting value (since that will be the max if nothing else is)


-- scanl and scanr = foldl and foldr, but they list intermediate states
-- (Seems handy for testing purposes!)

-- How many roots can you add up before your sum is >1000?

sqrtSums = length (takeWhile (<1000) (scanl1 (+) (map sqrt [1..]))) + 1

-- We could also retrieve the square root of the last number found to get our "last root" and our answer,
-- but I don't know whether that would be faster (I think it would be less readable)


-- $ = function application = lowest-priority operator, can replace parens

app = sqrt $ 3 + 4 -- 49

appMap = map ($ 4) [(4+),(2*),(^2),(sqrt)] -- [8.0,8.0,16.0,2.0]



-- Function composition = f(g(x)), binding two functions together

-- . :: (b -> c) -> (a -> b) -> a -> c -- enter an a, it becomes a b, which becomes a c
-- f . g = \x -> f (g x)

-- -- Example: Turn a lot of numbers negative
-- comp = map (negate . abs) [5, -3, -6, 7] -- everything becomes negative

-- compTwo = map (negate . sum . tail) [[1..5],[3..6],[1..7]] -- Returns the negative sum of all numbers but the first for each list

-- To quote the book:
-- sum (replicate 5 (max 6.7 8.9)) can be rewritten as 
-- (sum . replicate 5 . max 6.7) 8.9 
-- or as sum . replicate 5 . max 6.7 $ 8.9


-- Writing functions with currying = "point free style"
-- We can turn this:

-- fn x = ceiling (negate (tan (cos (max 50 x))))

-- Into this:

fn = ceiling . negate . tan . cos . max 50

-- Once parens are gone, we can also safely get rid of x
-- otherwise, cos (max 50) wouldn't make sense


-- See chapter's end for examples of concise functions written for oneself
-- vs. longer functions written to be understandable to others
-- (Being able to name lots of variables helps!)

-- showList = print functionList
-- MAKE THE ABOVE THING WORK!













-- REVIEW ALL THIS AS SOON AS YOU FINISH THE CHAPTER, IT IS IMPORTANT
