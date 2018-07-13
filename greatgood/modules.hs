-- Modules in Haskell

import Data.List

-- implementing nub for fun (in reality, nub is a special case of nubBy, which lets you define your own equality function)
-- (for example, instead of excluding numbers that equal another number in the list, you could exclude numbers that are the same as another number
-- in the list to within two decimal points, or eliminate all lists that have a value in common, etc.)

nub' :: (Eq a) => [a] -> [a]
nub' [] = []
nub' (x:xs) = (if elem x xs then nub' xs else x : nub' xs)

numUniques :: (Eq a) => [a] -> Int
numUniques = length . nub' -- "nub" creates a list of unique elements within a list
-- We could also use just plain nub here, since it comes in with Data.List


-- You can also load modules inside ghci:
-- :m + Data.List Data.Map Data.Set (and so on)

-- Selective function import:
-- Data.List (nub, sort)
-- Data.List hiding (nub) = everything but nub

-- import qualified Data.Map
-- The above brings in Data.Map functions, but we have to write them as
-- Data.Map.filter (etc.) -- this is good when multiple modules have identically-named functions
-- To shorten things, we also have:
-- import qualified Data.Map as Map (to call Map.filter) 


-- Data.List is all about lists (Prelude exports some of it automatically, but not all of it)

i = intersperse '.' "HOVA" -- "H.O.V.A" (no . at end)

cal = intercalate [0,1] [[1,2],[3,4],[5,6]]
-- [1,2,0,1,3,4,0,1,5,6]

t = transpose [[1,2,3],[4,5,6],[7,8,9]] -- Rows become columns! Flip the matrix 90 degrees
-- [[1,4,7],[2,5,8],[3,6,9]]  
-- This is good for polynomials -- lets you easily group your x^3, x^2, etc.

poly = map sum $ transpose [[0,3,5,9],[10,0,0,9],[8,5,1,-1]]  
-- [18,8,6,17] (transposing creates four groups of three numbers, one for each power)

-- concat flattens a list of lists, concatMap maps first and then fattens

-- and returns True only if a whole list is True

tf = and $ map (>3) [2,3,4] -- False
tof = or $ map (>3) [2,3,4] -- True

-- any // all = or // and, but for non-Boolean values

aNy = any (==4) [1,2,3,4] -- True
aLl = all (==4) [3,4,4,4] -- False


-- iterate makes an infinite list from a function and starting value

iter = take 4 $ iterate (*2) 1 -- [1,2,4,8]


split = splitAt 5 "AaronGertler" -- ("Aaron","Gertler")
splitLet = let (a,b) = splitAt 5 "AaronGertler" in b ++ a -- "GertlerAaron"


-- And there's takeWhile, of course, which is our best friend where infinite lists are concerned

-- #QUESTION: What differentiates an infinite list from something like a "while" loop in another language? Or are they essentially the same thing?

-- dropWhile = opposite of takeWhile, gives us everything *after* we hit a "False" value


-- span returns the takeWhile result and everything else, in two pieces

sp = let (first,rest) = span (<3) [1,2,3,4,5] in [first,rest]
-- [[1,2],[3,4,5]]

-- break = span (not . p)

br = let (first,rest) = break (>3) [1,2,3,4,5] in [first,rest]
-- [[1,2,3],[4,5]]


gr = group [3,1,1,3,3,2,2,1] -- [[3],[1,1],[3,3],[2,2],[1]]
-- Only groups equal elements if they are adjacent!
-- But we can fix that with a sort:

count = map (\(x:xs) -> (x, length (x:xs))) . group . sort $ [1,2,2,1,3,3,1,4,4,1,4]
-- [(1,4),(2,2),(3,2),(4,3)]
-- First, sort and group the list (into a list of lists)
-- Second, take each sublist and return a tuple, giving the first item in the list and the length of that list (that is, the number we have of that particular item) 

-- inits and tails are recursive versions of init and tail
its = inits "abc" -- ["","a","ab","abc"]
ils = tails "abc" -- ["abc","bc","c",""]

-- Searching a list for a sublist:
search :: (Eq a) => [a] -> [a] -> Bool
search target space =
  let tlen = length target
  in  foldl (\acc x -> if take tlen x == target then True else acc) False (tails space)
-- "tails" lets you look at all the places you could "start looking" in the space
-- So you just check all those tails to see if they start with the target value

-- isInFixOf does the same thing as our search function above
-- isPrefixOf and isSuffixOf just check the beginning/end for the target


-- Partition: split a list into lists that do/don't fit a predicate
par = partition (>3) [2,4,5,1,6,3,7] -- ([4,5,6,7],[2,1,3])
-- Unlike span/break, won't stop after hitting the first match


-- find grabs the first matching value from a list
fin = find (>4) [1,2,3,4,5,6] -- returns Just 5 (Maybe value)
finNothing = find (>4) [1,2] -- returns Nothing
-- finType = :t find = find :: (a -> Bool) -> [a] -> Maybe a
-- Takes data of any one type, needs a true/false predicate and a list to return a Maybe value
-- find is much safer than trying to return the head of a list of elements that match a predicate,
-- since taking the head of an empty list (if nothing matches) will return an error


-- elemIndices tells you where your elements are
indices = 'e' `elemIndices` "Where are the e's?" -- [2,4,8,12,14]


-- zip3 and zipWith3 zip three lists at a time, with/without a three-predicate function
-- We have these specialty zips up to 7


-- Working with strings!
lin = lines "first\nsecond\nthird" -- ["first","second","third"]
unlin = unlines ["first","second"] -- "first\nsecond" (string, not list)
wrd = words "first second third" -- ["first","second","third"]
unwrd = unwords ["first","second"] -- "first second" (string, not list)

dlt = delete 'e' "delete" -- "dlete" (just first occurrence of an element)
slashes = "delete" \\ "ete" -- "del" 
slashesNum = [1..10] \\ [1,5] -- [2,3,4,6,7,8,9,10]
unn = union [1..5] [3..7] -- [1,2,3,4,5,6,7] (joins all elements in one or both, eliminates duplicates)
sect = intersect [1..5] [3..7] -- [3,4,5] (only elements that were in both)