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