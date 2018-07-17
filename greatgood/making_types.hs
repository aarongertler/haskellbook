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