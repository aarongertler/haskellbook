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


-- Guards are a variant on patterns -- dictate responses based on data you receive, without rewriting the function name

sibResponse :: String -> String -- How could I make this function take in any kind of input? #Question
sibResponse sib
   | sib == "Sarah" = "Hey there, Sare Bear!"
   | sib == "Eli" = "What's new, brother?"
   | otherwise = "You're not my sibling!"

max' :: (Ord a) => a -> a -> a  -- Works with anything that has a defined order (can be compared with > and <)
max' a b   
    | a > b     = a  
    | otherwise = b  

-- Using "where" to define variables and make functions super-easy to read and change

mtgPrice :: (RealFloat a) => a -> a -> String
mtgPrice usd tix
   | price < cheap = "This is a budget deck!"
   | price < normal = "This is kind of pricey."
   | price < expensive = "We're breaking the bank with this one, huh?"
   | otherwise = "Guess I'm taking out a second mortgage."
   where price = min usd (tix * 2) -- Use input variables to define our "true" input variable
         -- cheap = 100
         -- normal = 200
         -- expensive = 500
         (cheap, normal, expensive) = (100, 200, 500)

-- Using where to grab parts of the input:

initials :: String -> String -> String
initials first last = [f] ++ ". " ++ [l] ++ "."
    where (f:_) = first
          (l:_) = last -- Grab first letter of each string with "where"

-- You can even define functions in where blocks:

pricelist = [(150, 50), (250, 160), (15000, 800)]

calcBudget :: (Integral a) => [(a, a)] -> [a] -- Start with a list of pairs, get a list of singletons
calcBudget list = [price usd tix | (usd, tix) <- list]
     where price u t = min u (t*2)

-- The "let" version of the above:
-- calcBudget list = [price | (usd, tix) <- list, let price = min usd (tix*2)]


-- let bindings are local -> they don't span across guards

testLet = (let (a,b,c) = (1,2,3) in a+b+c)
squareOne = [let square x = x * x in (square 5, square 3)] -- [(25,9)]
squareTwo = [let square = "Square" in (square ++ " " ++ "Triangle")]
-- Note that we can use square as a variable twice without problems
-- ... but the below makes Haskell angry:
-- square = "Square"
-- square x = x * x


-- Haskell also has a case function, but it doesn't seem to add much to the where and let options


-- Main difference between "let" and "where": Readability and style
-- https://wiki.haskell.org/Let_vs._Where
-- Sharing bindings with where is sometimes helpful, but specifying scope with let is also sometimes helpful
-- Also, watch out for where slowing your code (section 4 of the link)
