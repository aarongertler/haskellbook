-- Now we get to write some code! 

-- The first thing that's novel to me is the notion of an infix function.

-- div 8 4

-- 8 `div` 4

-- Both of those will have the same result! Very legible.

doubleMe x = x + x

doubleUs x y = (x + y) * 2

doubleAll x y z = doubleMe x + doubleMe y + doubleMe z

doubleSmall x = if x > 10 then x else x * 2 -- "else" is mandatory in Haskell, since every funcion *must* return something

stringFn = "Hello, I'm a long string for which stringFn is now a shortcut!"

list = [1, 2, 3]

sumlist = [1,2,3] ++ [4,5,6]

combo = ['c', 'o', 'm', 'b'] ++ ['o']

-- Be careful with ++, all elements in first list must be looked at before adding a seond half

smallCat = 'A':" SMALL CAT" -- "A SMALL CAT"

-- Adding to the end of a list requires another list, but regular numbers and strings can be added to the front of a list

-- [1,2,3] is syntactic sugar for 1:2:3:[]   (interesting!)

extract = "String extraction" !! 7 -- "e", returns indexed character

-- Lists can only contain instances of one data type

-- head [1,2,3,4] -- 1
-- tail [1,2,3,4] -- [2,3,4]
-- last [1,2,3,4] -- 4
-- init [1,2,3,4] -- [1,2,3]
-- length [1,2,3,4] -- 4
-- null [] -- true
-- reverse [1,2,3,4] -- [4,3,2,1]
-- take 2 [1,2,3,4] -- [1,2]
-- take 5 [1,2,3,4] -- [1,2,3,4]
-- drop 2 [1,2,3,4] -- [3,4] (Removes first two elements)
-- drop 5 [1,2,3,4] -- []
-- minimum [1,2,3,4] -- 1
-- maximum [1,2,3,4] -- 4
-- sum [1,2,3,4] -- 10
-- product [1,2,3,4] -- 24
-- 3 `elem` [1,2,3,4] -- True
a = [1..4] -- [1,2,3,4]
b = ['g'..'j'] -- ['g','h','i','j']
c = [2,5..14] -- [2,5,8,11,14]
d = [2,5..16] -- [2,5,8,11,14] -> doesn't matter if our last number doesn't fit our "step"
e = [20,19..1] -- Reverse list from 20 to 1, you do need that 19

-- Don't use floats in list ranges, that stuff gets weird

f = take 24 [13,26..] -- Returns first 24 multiples of 13!

g = take 10 (cycle [1,2,3,4]) -- [1,2,3,4,1,2,3,4,1,2]

h = take 5 (repeat 1) -- [1,1,1,1,1]
replicate 1 5 -- [1,1,1,1,1]
 