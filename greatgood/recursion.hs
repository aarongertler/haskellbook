-- Recursion in Haskell

-- Sample recursive "maximum" function -- pattern-matching makes it easy!

maximum' :: (Ord a) => [a] -> a -- Need a list of things that can be ordered
maximum' [] = error "this list is empty"
maximum' [x] = x
-- maximum' (x:xs) -- Describes any list with more than one item
--     | x > maxTail = x
--     | otherwise = maxTail
--     where maxTail = maximum' xs -- If the first element in our list isn't the biggest, the biggest will be the max of the rest of the list
maximum' (x:xs) = max x (maximum' xs) -- faster if we use the handy max function


replicate' :: (Num a, Ord a) => a -> b -> [b] -- Num isn't a subclass of Ord; complex numbers, for example, can't be ordered with others
replicate' n x
    | n <= 0 = []
    | otherwise = x:replicate' (n-1) x -- Can't believe we get out of this with so few parentheses!


take' :: (Num a, Ord a) => a -> b -> [b]
take' n _ -- We can take from... anything?
    | n <= 0 = [] -- Can't take a negative # of things from a list (no otherwise needed, positive n gets handled below)
take' _ [] = [] -- Can't take anything from an empty list
take' n (x:xs) = x : take' (n-1) xs -- Rebuild the list we started with until we have enough pieces


reverse' :: [a] -> [a] -- Any list of any kind of thing works
reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x] -- Build the new list from back to front


-- Infinite recursion is often fine!

repeat' :: a -> [a]
repeat' x = x : repeat' x -- Infinite list created

finito = take 5 (repeat 3) -- We never need to finish the infinite list; 
-- thanks to lazy eval, we just look at the first 5 things and find that all are 3s


elem' :: (Eq a) => a -> [a] -> Bool -- Was able to write this one without looking, good
elem' a [] = False
elem' a (x:xs)
    | a == x = True
    | otherwise = elem' a xs


quicksort :: (Ord a) => [a] -> [a] -- Helpful illustration for this in the book
quicksort [] = []
quicksort (x:xs) =  -- x is always placed with the correct number of bigger and smaller elements on each side
	let smaller = quicksort [a | a <- xs, a <= x] -- We could also have a < x here and a >= x below
	    bigger = quicksort [a | a <- xs, a > x]
	in  smaller ++ [x] ++ bigger

