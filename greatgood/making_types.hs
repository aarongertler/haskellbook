-- Making our own types and typeclasses

import qualified Data.Map as Map 

-- data Bool = False | True   -- Show all options when defining a type

-- Custom type:
-- data Shape = Circle Float Float Float | Rectangle Float Float Float Float deriving (Show)
data Point = Point Float Float deriving (Show)
data Shape = Circle Point Float | Rectangle Point Point deriving Show -- two corners give you one rectangle
-- Honestly, I sort of like the pre-Point version more, but perhaps it's better to
-- keep more aspects of the code "explicit", less guesswork for other readers that way


-- We need three numbers (center x/y and radius) to define a circle,
-- and four coordinates to define a rectangle

-- :t Circle :: Float -> Float -> Float -> Shape


-- Using our custom type to create functions:

area :: Shape -> Float -- Circle isn't a type, shape is
area (Circle _ r) = pi * r ^ 2 -- Pattern match against multiple shapes (but only one would still work)
area (Rectangle (Point x1 y1) (Point x2 y2)) = (abs $ x2 - x1) * (abs $ y2 - y1)

-- To use this function, we'd need something like:
-- area $ Circle 10 10 10
-- ...and that's a bit ugly, so let's simplify with deriving (Show), above
-- We still can't use "area Circle" without $, but now we can write "Circle 10 20 5" and have it show in GHCI

circles = map (Circle (Point 0 0)) [4,5,6] -- creates three circles (same center, different radii)

nudge :: Shape -> Float -> Float -> Shape -- move a shape x to the right and y up
nudge (Circle (Point x y) r) a b = Circle (Point (x + a) (y + b)) r
nudge (Rectangle (Point x1 y1) (Point x2 y2)) a b = 
       Rectangle (Point (x1 + a) (y1 + b)) (Point (x2 + a) (y2 + b))


-- default coordinates give us another way to build
baseCircle :: Float -> Shape
baseCircle r = Circle (Point 0 0) r

baseRectangle :: Float -> Float -> Shape
baseRectangle width height = Rectangle (Point 0 0) (Point width height)

recTwo = baseRectangle 2 2
recNudge = nudge (baseRectangle 2 2) 4 4


-- If we built a module for this:
-- module Shapes
-- ( Point(..)
-- , Shape(..) -- Equivalent to Share (Rectangle,Circle) -- gets all the constructors
-- , area
-- , nudge
-- , baseCircle
-- , baseRectangle
-- ) where

-- even if we didn't export the constructors directly, people could still create
-- Rectangles and Circles using baseRectangle and baseCircle


-- Next example shows creation of a "Person" type, confirming my suspicion that this is one way that
-- Haskell does object-oriented stuff
-- Shows off data-fetching functions, for example

data Person = Person { firstName :: String
                     , lastName :: String
                     , age :: Int
                     , height :: Float
                     , phoneNumber :: String
                     } deriving (Show) -- don't forget this part!

showPerson :: Person -> String
showPerson (Person {firstName = f, lastName = l, age = a}) =
  "My name is " ++ f ++ " " ++ l ++ ", age " ++ show a

-- if we try to leave out some fields, we'll get a non-fatal error (can still load module)
-- but placing fields in any order is fine
-- e.g. Person {lastName="Gertler",firstName="Aaron"}... etc.


-- This "record syntax" is only needed if we have multiple fields that could be confused
-- good for something like a Person, not needed for something like a Point (if we always remember x,y order)


-- Value constructor = takes values, produces value (e.g. Person)
-- Type constructor = takes types, produces type (e.g. Maybe, List)

-- tidbit: Nothing and [] are very flexible -- we can have them act like a (Maybe a) for any type a 
-- and a list of any type, respectively. [1]++[] and ['a']++[] both work for that reason


-- Anyway, type parameters are best when we want to work with many different types
-- (For example, we might have a list of Ints or Chars or Strings...)
-- Map also does this, letting us map any Ord type to any other Ord type with k, v

-- Even so, we don't specify data (Ord k) => Map k v = ...
-- That's because, when we want to order keys in a function, we'll specify (Ord k) in that function anyway
-- and when we don't want to order keys, we can avoid needing to add (Ord k)
-- unless (Ord k) is baked into our data declaration, in which case we'll need to add it to *every map function we write*

data Vector a = Vector a a a deriving (Show, Read)  
  
vplus :: (Num t) => Vector t -> Vector t -> Vector t  
(Vector i j k) `vplus` (Vector l m n) = Vector (i+l) (j+m) (k+n)  
-- We don't use type Vector t t t -> t, because our types are based on our *constructor",
-- which we've defined as Vector a (inputting Vector t as a type will let us use Vector t t t)


-- Typeclasses = interfaces, not Python-ish "classes"
-- Make a data type first, then think about how it can act to choose typeclasses (Eq, Ord, etc.)

-- We can use "deriving" on Show, Enum, Bounded, Read, Eq, and Ord
-- If we use Eq for our type, Eq must also work for all types *within* our type (same for some other types)
-- Enum = predecessors and successors, Bounded = lowest and highest possible value

-- Show and Read let us turn values of our type into a string, and turn strings into values of our type (respectively)
-- Trying to read Vector seems to fail no matter how I slice it (#QUESTION) (seems to happen because Vector isn't actually a type, but a type function mapping numbers to vectorrs)


data Day = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday   
           deriving (Eq, Ord, Show, Read, Bounded, Enum)  
-- Gives us a chance to test all the types (e.g. sun = read "Sunday" :: Day)
-- [minBound .. maxBound] :: [Day] -> creates list of all days from "least" (Monday) to "most" (Sunday), thanks to Enum


-- type String = [Char] -- doesn't invent a new type, just defines these two types as synonyms
type AssocList k v = [(k,v)] -- Lets us represent a list without defining a particular type for keys and values
-- Another example: type phoneBook = [(String,String)]
-- Better yet:
-- type PhoneNumber = String
-- type Name = String
-- type phoneBook = [(Name,PhoneNumber)] -> much more descriptive! 
-- This also lets us write cute functions like  
-- inPhoneBook :: Name -> PhoneNumber -> PhoneBook -> Bool    (much better than String -> String -> [(String,String)] -> Bool)
-- inPhoneBook name pnumber pbook = (name,pnumber) `elem` pbook


-- We can also use curried type constructors (partially applying them to get new constructors)
type IntMap = Map.Map Int -- Now we can use IntMap x  and map Int to x, whatever type that is


-- Useful type = Either a b, which is like Maybe a, but offering a more informative fail/secondary outcome case than "Nothing"

data LockerState = Taken | Free deriving (Show, Eq) -- We've defined an "either" here
type Code = String
type LockerMap = IntMap (LockerState, Code) -- we can now input an "either" and a string, and map that into being a coded locker that is either taken or free
                                            -- also, using "IntMap" here lets us give each locker a number in the map!


lockers :: LockerMap -- define the type of the variable before you try to map it!
lockers = Map.fromList   
    [(100,(Taken,"ZD39I"))  
    ,(101,(Free,"JAH3I"))  
    ,(103,(Free,"IQSA9"))  
    ,(105,(Free,"QOTSA"))  
    ,(109,(Taken,"893JJ"))  
    ,(110,(Taken,"99292"))  
    ] 

lockerLookup :: Int -> LockerMap -> Either String Code -- take a locker number and map of lockers to look at, give us back an "either" noting one of two things (our code, or a notice that the locker is taken)
lockerLookup lockerNumber map = 
  case Map.lookup lockerNumber map of -- define results for different things we could find checking this number on our map (lookup is pre-defined in Map)
      Nothing -> Left $ "Locker number " ++ show lockerNumber ++ " doesn't exist!"    -- lookup uses "Maybe", so we could get back a "Nothing" result
      Just (state, code) -> if state /= Taken -- this is why we brought in (Eq) above
                            then Right code -- we've defined that our either's Right is a number, so we specify that the number we return is in Right
                            else Left $ "Locker " ++ show lockerNumber ++ " is already taken!"

-- NOTE: This is one of the more complex things I've written in Haskell so far, and I'm already pining for Python... but trying to keep in mind that
-- Haskell is much faster underneath, and that the smooth "Nothing" type here *might* not have such a simple equivalent in Python


-- Recursive data structures: Their constructors have fields that include their own type (like lists!)

data List a = Empty | Cons a (List a) deriving (Show, Read, Eq, Ord) -- Enum and Bounded don't make sense here
-- In other words, every list is either an empty list or a head combined with a list 
-- Cons is a word for :, and is a constructor itself -- takes a value and list, returns a list

-- infixr 5 :-:    We declare a "fixity" for our constructor -- : is right-associative (infixr, not infixl) and has a fixity of 5
-- higher fixity = higher priority on binding. * is fixity 7, + is fixity 6 -> hence, order of operations

-- The chapater redesigns some basic list functions from scratch, might be good to revisit


-- Binary search tree time!

data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Show) -- if our node isn't empty, it has two trees branching off from it (though one of those trees could be empty)

singleTree :: a -> Tree a
singleTree x = Node x EmptyTree EmptyTree -- Create the top node of a tree

insert :: (Ord a) => a -> Tree a -> Tree a -- No pointers in Haskell, but making a new tree from scratch is still quick (hooray for lazy eval)
insert x EmptyTree = singleTree x -- make a new tree if there's nothing to which we can add
insert x (Node a left right) -- Check the node we're looking at, and...
    | x == a = Node x left right -- Just return the same tree we had before, no change
    | x < a  = Node a (insert x left) right -- return the same node, but now pushing x to the left (where it will eventually hit itself or an EmptyTree)
    | x > a  = Node a left (insert x right)

elemTree :: (Ord a) => a -> Tree a -> Bool
elemTree x EmptyTree = False -- If we hit the end of our path, our element can't exist -- otherwise, we'd have been guided there
elemTree x (Node a left right)
    | x == a = True
    | x < a = elemTree x left -- check with the node to the left, which is one level "down"
    | x > a = elemTree x right

buildTree :: (Ord a) => [a] -> Tree a
buildTree ls = foldr insert EmptyTree ls

t = buildTree [4,5,1,8,9,2,2,6]


-- Making our own typeclasses!
