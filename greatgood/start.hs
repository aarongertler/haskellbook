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
i = replicate 1 5 -- [1,1,1,1,1]
 
listC = [x*x | x <- [1..10]] -- X^2 from 1 to 10
listFilter = [x*x | x <- [1..10], x*x >= 25] -- Finds values of x^2 greater than 25 where x is between 1 and 10

listFn y = [if x < 10 then "Boom!" else "Bang!" | x <- y, even x] -- Returns Boom and Bang for even values of X when we provide a list of numbers y

-- listFn [7..13] -- ["Boom!", "Bang!", "Bang!"]

listExclude = [x | x <- [10..20], x /= 13] -- All X from 10 to 20 that aren't 13

listTwoVar = [x*y | x <- [3..5], y <- [4..6]] -- Returns each of 3,4,5 times each of 4,5,6
listTwoVarFilter = [x*y | x <- [3..5], y <- [4..6], x*y > 20] -- Places a filter on the results we want


-- Assigning lists outside of their list comprehensions:

adj = ["blue", "green"]
noun = ["bird", "frog"]

madLibs = [adj ++ " " ++ noun | adj <- adj, noun <- noun] -- returns blue and green frogs and birds


removeLower string = [char | char <- string, char `elem` ['A'..'Z']] 
-- The above takes a string and returns only chars that are ELEMents of the list A..Z


-- You can use nested list comprehensions:

xxs = [[1,3,5,2,3,1,2,4,5],[1,2,3,4,5,6,7,8,9],[1,2,4,2,1,6,3,1,3,2,3,6]]  
nestedC = [ [ x | x <- xs, even x ] 
                | xs <- xxs] -- Splitting over multiple lines helps!
-- The above says: "Look at each list in xxs, then spit out all even x from each of those lists"
-- (This will still keep the answer in the form of a list of lists)

-- zip allows the combination of lists
zipTest = zip [1..5] ['a'..'e'] --[(1, 'a'), (2, 'b'), etc.]
zipTestTwo = zip [1..] ["apple", "banana", "carrot"] -- We cut off our infinite list when we run out of finite things to zip with

-- Putting it all together:

limit = 100
rightTriangles = [(a,b,c) | c <- [1..limit], b <- [1..(c - 1)], a <- [1..(b - 1)], a^2 + b^2 == c^2]
-- [(3,4,5), (6,8,10)], and then lots more 

-- All you need is a starting set of data (our limit for c), and then lots of filters on top of that
-- Notably, we check each value of c in turn (making it easy to see which a and b values qualify to be checked)