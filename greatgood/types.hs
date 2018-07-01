-- Types and Typeclasses in Haskell

-- Play around with :t in GHCI (notably, 'a' and "a" are a char and a [char])
-- That's because char is one character, while [char] = string = a list of characters
-- True and 4 == 5 are both Bool (it's all about what they return)

-- :t on a function returns x -> y, where x is what you start with and y is what you get

addThree x y z = x + y + z

-- :t addThree :: Int -> Int -> Int ->  =  add three Ints together to make a fourth Int

factorial n = product [1..n]

-- factorial 5 is an Int, factorial 50 is an Integer (used for very big numbers)

-- Double is twice as precise as Float (gives lots more decimal points), and we can specify which one we want!

circumference :: Float -> Float
circumference r = 2 * pi * r

circumference' :: Double -> Double
circumference' r = 2 * pi * r

-- head :: [a] -> a  =  the head function works on any list and can return anything, so we define it with the generic "type variable" a
-- This makes head a *polymorphic* function (any function that has type variables)

-- fst uses two type variables: fst :: (a, b) -> a

-- even == is a function: :t (==) :: (Eq a) => a -> a -> Bool
-- => is a class constraint, showing that both values must have the same type
-- :t (elem) :: (Eq a) => a -> [a] -> Bool (check if a is in list [a])

-- Ord is for types that have an ordering: :t (>) :: (Ord a) => a -> a -> Bool

threeToStr = show 3 -- "3"
listFromStr = read "[1,2,3]" ++ [4] -- [1,2,3,4]
-- We need to *do* something with the result of read, or we won't know what type to return
-- ...unless we make that explicit

int = read "5" :: Int -- 5
float = read "5" :: Float -- 5.0
set = read "(3, 'a')" :: (Int, Char) -- (3, 'a')

-- Enum is a typeclass for anything usable in a range (Bool, Char, Int, etc.)

orderings = [LT ..] -- Awkwardly, we need that space here (but not for, say, numbers)
true = succ False -- True

-- Some types are bounded -- they have upper and lower limits

bounds = maxBound :: (Bool, Int, Char) -- (True,9223372036854775807,'\1114111')

-- Any function that accepts Num (e.g. *) accepts Int, Integer, Float, and Double
-- :t (*) :: (Num a) => a -> a -> a   We convert any mystery Num to the same data type as the other Num

result = 5 * (6 :: Int)

-- Integer is like bignum -- works for arbitrarily large numbers, but is slower than Int

-- fromIntegral = function that will take any number and change it to match the type of the other number
fromInt = fromIntegral (length [1,2,3]) + 3.2 -- 6.2, would break without fromIntegral
-- Works because we turned 3.2 from an integral to something "more general"

-- StackOverflow on this: https://stackoverflow.com/questions/23168462/i-dont-understand-t-for-fromintegral
-- If a is Integral and b is Num, fromIntegral can make b from a

fromIntAgain = fromIntegral (1 :: Int) + 2.2