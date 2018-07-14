-- Modules in Haskell

import Data.List
import Data.Char

import qualified Data.Map as Map
import qualified Data.Set as Set

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


-- the native "insert" command in Haskell only takes one parameter, and places it so it keeps a list "sorted"
-- (for example, 4 will be placed right before the first number in the list with value 4 or greater)
-- (same idea for any Ord-type variables)


-- Data.List includes methods like genericLength, which return Num rather than Int
-- and various other "generic" methods, which can be useful when we can't work with an Int
-- (e.g. when dividing by the result of one of those functions)


-- more generic grouping function: groupBy
vals = [1,-1,2,-2,-3,4,5,-6]
gb = groupBy (\x y -> (x > 0) == (y < 0)) vals
-- [[1,-1],[2,-2,-3],[4],[5,-6]]
-- If we had y > 0 here, we'd group all consecutive positives and negatives with each other
-- Instead, we split groups by the "equality test" of whether a given number is on the opposite side
-- of the pos/neg divide
-- So we start with 1, throw in -1 (which fits the equality), then start a new group with 2 (which doesn't fit the equality).
-- and start a new group every time we hit a positive number from then on
-- KEY IDEA: We always compare to the first number in the first group, not the most recent element in the list

-- groupBy                 :: (a -> a -> Bool) -> [a] -> [[a]]
-- groupBy _  []           =  []
-- groupBy eq (x:xs)       =  (x:ys) : groupBy eq zs
--                            where (ys,zs) = span (eq x) xs

-- So we run "span" with our function taking x (start of current sublist) as a parameter,
-- and attach x to ys (whatever matches the equality function with x), before running the same function on everything after ys 
-- (the part where the span cuts off, because we've hit something that didn't fit our equality function)


-- "on" lets you clean up groupBy by automating that function structure
on' :: (b -> b -> c) -> (a -> b) -> a -> a -> c
-- take a two-parameter function and a one-parameter function, plus at least two items that can be parameters, 
-- and return something the first function could return (second function must return parameters the first function can use)
on' f g = \x y -> f (g x) (g y)  
-- When we run "on", we take our parameters and see how g (x) and g (y) compare, using f as our comparison function

gbOn = groupBy ((/=) `on'` (>0)) vals
-- [[1,-1],[2,-2,-3],[4],[5,-6]]


-- sortBy lets you choose your own sorting mechanism
lists = [[1,2],[1,2,3],[1],[]]
sb = sortBy (compare `on'` length) lists -- returns the list of lists sorted by length
-- softBy takes the LT/EQ/GT results of compare and moves list items around as a result



-- Let's work with characters! Thanks, Data.Char
-- Building blocks: isSpace, isLower, isUpper, isAlphaNum, etc.
-- also toUpper, toLower...

allAN = all isAlphaNum "Aaron Gertler the 1st" -- False, spaces don't count

-- "general categories" define types of characters

gc = map generalCategory " \nA9?|"
-- [Space,Control,UppercaseLetter,DecimalNumber,OtherPunctuation,MathSymbol]

-- We can take apart strings of numbers automatically!
dti = map digitToInt "34538" -- [3,4,5,3,8]
itd = intToDigit 15 -- 'f', and 5 becomes '5', 10 becomes 'a', etc.
o = ord 'a' -- 97
c = chr 97 -- 'a' (fun with Unicode)


-- Caesar cipher in Haskell:

caesar :: Int -> String -> String
caesar shift msg =
  let ords = map ord msg
      shifted = map (+shift) ords
  in  map chr shifted 
-- first, define "shifted" (the string converted to numbers, with the cipher added)
-- then, return the result of mapping "chr" onto these shifted numbers

decode :: Int -> String -> String
decode shift msg = caesar (negate shift) msg
-- runs "caesar" again, but using a cipher that is the inverse of the previous cipher


-- Dictionaries in Haskell

dict = [("key","value")
       ,("keytwo","valuetwo")
       ,("keythree","valuethree")]

findKey :: (Eq k) => k -> [(k,v)] -> Maybe v
findKey key [] = Nothing -- using Maybe data types in case our key isn't really in the dictionary
findKey key ((k,v):xs) = if key == k -- Specify that head of list will be a key/value pair
                            then Just v
                            else findKey key xs

-- This is a classic fold pattern (operating on the head of a list, then recurring on the tail)
-- So let's try and build it with a fold

foldKey :: (Eq k) => k -> [(k,v)] -> Maybe v
foldKey key = foldr (\(k,v) acc -> if key == k then Just v else acc) Nothing
-- This is foldr because we return the last value for our acc and want that to be the leftmost ("first") value in the list
-- Always include acc as a parameter!
-- Folds are usually easier to read than explicit recursions (for anyone who knows Haskell)
 

-- To make mapping more efficient, use Map.fromList on your k/v lists
-- and turn them into a map type
-- Map.toList is the inverse of fromList, turning maps into lists

mp = Map.insert 2 4 . Map.insert 7 48 . Map.insert 5 25 $ Map.empty

-- fromList makes use of insert to create maps
-- k must be an ord, because Map functions use trees of orderable values

fromList' :: (Ord k) => [(k,v)] -> Map.Map k v
fromList' = foldr (\(k,v) acc -> Map.insert k v acc) Map.empty
-- foldr used because we want the inserts to be clustered together and performed all at once on the empty list (#QUESTION: Is this true?)

mapsize = Map.size mp -- 3 (# of k/v pairs)
mps = Map.singleton 3 9 -- creates map with one k/v pair

-- Map.map and Map.filter operate on all values in maps (not on keys)


-- fromListWith turns lists into maps -- but doesn't drop duplicate keys
-- instead, we can tell Haskell what to do with duplicates 
-- (e.g. storing all values for a key together, adding values together, keeping the largest value for a given key...)


phoneBook =   
    [("betty","555-2938")  
    ,("bonnie","452-2928")  
    ,("patsy","493-2928")  
    ,("patsy","943-2929")  
    ,("patsy","827-9162")  
    ,("lucille","205-2928")  
    ,("wendy","939-8282")  
    ,("penny","555-2111")]  

phoneBookToMap :: (Ord k) => [(k, String)] -> Map.Map k String  
phoneBookToMap xs = Map.fromListWith (\number1 number2 -> number1 ++ ", " ++ number2) xs
-- Won't just combine one pair of values -- combines all pairs
-- So we'll have ("patsy", "493-2928, 943-2929, 827-9162")

-- Alternative method (shorter, maybe a bit less readable?)
-- phoneBookToMap :: (Ord k) => [(k, a)] -> Map.Map k [a]  
-- phoneBookToMap xs = Map.fromListWith (++) $ map (\(k,v) -> (k,[v])) xs  



-- Meaning of the below:
-- fromListWithKey inserts k/v pairs one by one, performing a function on each
-- fromListWith does the same thing, without saving the key value
-- insertWithKey is the function that *actually* checks for duplicate keys and operates the combining function
   -- ...and unfortunately, it uses semi-Assembly code that I can't parse
   -- (though I can imagine how it does what it does easily enough)

-- fromListWith :: Ord k => (a -> a -> a) -> [(k,a)] -> Map k a 
-- fromListWith f xs
--   = fromListWithKey (\_ x y -> f x y) xs

-- fromListWithKey :: Ord k => (k -> a -> a -> a) -> [(k,a)] -> Map k a 
-- fromListWithKey f xs 
--   = foldlStrict ins empty xs
--   where
--     ins t (k,x) = insertWithKey f k x t



-- Finally, we have sets (only unique values, always ordered):

text1 = "The quick brown fox jumps over the lazy dog."
text2 = "Pack my box with five dozen liquor jugs!"

set1 = Set.fromList text1
set2 = Set.fromList text2
inter = Set.intersection set1 set2
diff1 = Set.difference set1 set2 -- what's in set1, but not set2?
union = Set.union set1 set2 -- what's in one OR the other set?

-- Many other functions are similar to those for maps:
-- singleton, insert, null, delete, size...
-- sets can be filtered and mapped, too

-- To check for subsets, use 'isSubsetOf'