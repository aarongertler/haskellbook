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