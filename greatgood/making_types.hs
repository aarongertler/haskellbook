-- Making our own types and typeclasses

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