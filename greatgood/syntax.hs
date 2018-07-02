-- Syntax in Functions

-- Haskell's version of a case function = pattern-matching:

lucky :: (Integral a) => a -> String -- Takes an integral, outputs a string (integrals are whole numbers, like Int or Integer)
lucky 7 = "That's a seven!"
lucky x = "Not a seven." -- Will only work for other integers and Ints

-- More pattern-matching!

factorial :: (Integral a) => a -> a -- Stops us from accidentally entering an infinite loop, e.g. with "factorial 5.1"
factorial 0 = 1
factorial n = n * factorial (n - 1)


-- Using pattern-matching to be very specific and define a clear addVectors expression:

addVectors :: (Num a) => (a, a) -> (a, a) -> (a, a) -- Take two pairs of numbers, return a third pair
addVectors (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

-- You can also pattern-match with list comprehensions (definining the type of data you'll see in the course of explaining what to do with that data)

xs = [(1,2), (3,4), (5,6)]
d = [a+b | (a,b) <- xs] -- [3. 7. 11]

-- use x:xs in a pattern to store the first item of your list in x, and the rest in xs
-- Or grab the first three variables with something like x:y:z:xs

-- Writing our own head function with this knowledge:

head' :: [a] -> a -- Start with a list, return anything
head' [] = error "This list is empty" -- Define error message
head' (x:_) = x -- We use _ to show that we don't care about the rest of the list (no need to give it a variable of its own)

-- Recursive length function:

length' :: (Num b) => [a] -> b -- Take a list, return a Num (Int is the wrong kind of thing to go here... for some reason #Question)
length' [] = 0
length' (_:xs) = 1 + length' xs -- Works for anything that isn't an empty list

-- Using @ to break things into patterns:

firstLetter :: String -> String -- Take a string, return a string
firstLetter "" = "Empty string"
-- firstLetter (x:xs) = "The first letter of " ++ (x:xs) ++ " is " ++ [x]
firstLetter all@(x:xs) = "The first letter of " ++ all ++ " is " ++ [x]
-- The two patterns above give the same result, but second lets us not write x:xs twice
