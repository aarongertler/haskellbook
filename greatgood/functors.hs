-- Functors and Applicative Functors

-- Open typeclaases and a wide range of types = high-level polymorphism! 
-- We can define typeclasses for very general/abstract behavior

-- As a reminder, functors are things we can map over (lists, maybes, trees, etc.)
-- That's where we get the fmap function
-- Also, remember that any type constructor we make into an instance of Functor is * -> * (e.g. Maybe works, because String -> Maybe String)
-- And to use something like Either, we can only make (Either a) into a functor, ignoring its other half
-- fmap :: (B -> c) -> Either a b -> Either a c

-- Another functor type: IO (is * -> *, because we can have IO String for grabbing strings with IO actions)

instance Functor IO where    -- fmap :: (a -> b) -> IO a -> IO b
	fmap f action = do -- for example, we could fmap "words" onto "getline", get a line, then split it into words
		result <- action
		return (f result) -- returns an IO action (result), so we know we need "do" syntax

main = do line <- fmap reverse getline
    putStrLn $ "You said " ++ line ++ " backwards!"

-- main = do line <- fmap (intersperse '-' . reverse . map toUpper) getline
--     putStrLn line

-- Why do we need the fmap for this? Could some other function work just as well for a simple transformation? 
-- Well, we could bind the I/O result to a name, then apply the function to the name, but this is more elegant


-- Now for (->) r!

instance Functor ((->) r) where
	fmap f g = (\x -> f (g x)) -- Mapping a function over another function! We produce a function (\x ->) that runs function f over the result of (g x)
	                           -- Which, as we're about to see below, is just function composition!

-- Another way to think of this: 
-- fmap :: (a -> b) -> ((->) r a) -> ((->) r b), or, in other words...
-- fmap :: (a -> b) -> (r -> a) -> (r -> b)  = we take a function that turns a -> b and a function that turns r -> a, produce a function that turns r -> b

-- So a much simpler version of the above:
-- instance Functor ((->) r) where
-- 	fmap = (.)

-- fmap (*3) (+100) 1 = 303   -- Think of (+100) as a "box" that will eventually hold a result, then use (*3) to change what's in the "box"
-- fmap (show . (*3)) (+100) 1 = "303"


-- Functions that work on any functor will work differently depending on the functor we choose:

-- "The type fmap (replicate 3) :: (Functor f) => f a -> f [a] means that the function will work on any functor. 
-- What exactly it will do depends on which functor we use it on. 
-- If we use fmap (replicate 3) on a list, the list's implementation for fmap will be chosen, which is just map. 
-- If we use it on a Maybe a, it'll apply replicate 3 to the value inside the Just, or if it's Nothing, then it stays Nothing."

-- fmap (replicate 3) (Right "foo") = Right ["foo","foo","foo"]


-- First law of functors: fmap id = id (easy to understand)
-- Second law of functors: fmap (f . g) = fmap f . fmap g, or: fmap (f . g) F = fmap f (fmap g F) = simple property of function composition

-- What's an example of something that *doesn't* obey one of these laws? 
-- Here we go!

data CMaybe a = CNothing | CJust Int a deriving (Show)
-- C = "counter", but the Just part holds two fields, one an Int and one free to be anything
-- If we build fmap, and we wanted something to happen to both Int and a...
  -- ...that would mean that "fmap id" won't actually give back the same thing we put in (breaking law #1),
  -- which means that CMaybe isn't really a functor, and could produce faulty code 
  -- The only way we could use CMaybe as a functor is if Int stayed the same when we used fmap

-- Lesson: If you build a functor, always check it against the two laws!

-- #QUESTION: Make sure you can define the following words later: "functor", "type", "type constructor"


-- And now, onto applicative functors...

-- fmap (*) (Just 3) = Just ((*) 3) = Just (* 3) = Num a => Maybe (a -> a) = function wrapped in a Just!

-- If we map "compare", which has a type of (Ord a) => a -> a -> Ordering over a list of characters, 
-- we get a list of functions of type Char -> Ordering (compare 'a', compare 'b', compare 'c'... all will work if we add a second character for comparison)

-- Applicative lets us map something like Just (3 *) over Just 5, where normally we'd be out of luck (fmap maps normal functions over functors, not "functor functions" like Just (3 *))

class (Functor f) => Applicative f where -- Anything that is in Applicative is in Functor, so fmap will work
	pure :: a -> f a -- take any value and return a functor with that value inside of it
	(<*>) :: f (a -> b) -> f a -> f b -- Takes a functor containing a function (like Just (3 *)) and maps the contained function over (f a) to produce (f b)

instance Applicative Maybe where
	pure = Just -- takes something like 3 or "dog" and produces Just 3 or Just "dog"
	Nothing <*> _ = Nothing -- can't extract a function from Nothing
	(Just f) <*> something = fmap f something -- whatever we want to apply our Just function to, we can! This "extracts" our function easily. 
	  -- We'll customize the "extract" function for different applicative functors

	  -- Just (+3) <*> Just 9 = Just 12
	  -- pure (+3) <*> Just 9 = Just 12  -> this version is preferred when you are using <*>
	  -- pure (+) <*> Just 3 <*> Just 5 = Just 8   -> Look how much stuff you can smash together with applicative functors!

	-- Applicative law: pure f <*> x = fmap f x
	-- Or we can use the version of this that already exists:

(<$>) :: (Functor f) => (a -> b) -> f a -> f b
f <$> x = fmap f x -- We know we have a functor, which simplifies things

-- (++) <$> Just "Aaron " <*> Just "Gertler" = Just "Aaron Gertler"   -> Making functor functions almost as simple as regular functions!


-- Another applicative functor: Lists!

instance Applicative [] where
	pure x = [x] -- Need singleton list, not empty list (a value, not the lack of a value)
	fs <*> xs = [f x | f <- fs, x <- xs] -- for each function in list f, apply it to all the elements in list x 
	-- See list syntax in start.hs, the stuff after the divider defines what we run through our f x (in this case, everything in each list)
	  -- Note: If we only have one function in our list, this is just mapping

-- [(+),(*)] <*> [1,2] <*> [3,4] = [(1+),(2+),(1*),(2*)] <*> [3,4] = our final list of eight elements
-- filter (>5) $ [(+),(*)] <*> [1,2] <*> [3,4] = [6,6,8]

-- Lists are "non-deterministic computations" = we get all possible results, not just one result
-- Adding non-deterministic computations gives us an even bigger (and less certain!) computation 

instance Applicative IO where
	pure = return -- works as a minimal context with a value, since "return" *does* create an I/O action
	a <*> b = do
		f <- a
		x <- b
		return (f x)

-- This lets us use fun shorthand! For example:

addLine :: IO String -- working with I/O actions that will only involve strings
addLine = (++) <$> getLine <*> getLine -- map (++) onto the result of our first getLine, apply that concatenation to the second getLine
  -- This means we don't have to bind each getLine to a variable, or produce an explicit "return" statements.
  -- Longer version:

  -- myAction :: IO String  
  -- myAction = do  
  --   a <- getLine  
  --   b <- getLine  
  --   return $ a ++ b  

instance Applicative ((->) r) where
	pure x = (\_ -> x) -- minimal context = function that takes anything, always returns the same result
	f <*> g = \x -> f x (g x) -- for example, bind +3 and *3 so that you return (x * 3) + 3
		-- #QUESTION: Why does entering "5" below create numbers 8 and 500 to be added, rather than returning (+3) ((*100) 5)? (Adding a 5 after (+3) would be incoherent...)

-- (+) <$> (+3) <*> (*100) $ 5 = add (x+3) and (x*100) to make one function, then apply that function to 5, returning (8+500)
-- The "5" only represents what is entered into our list of functions (we get 8 and 500 separately, not 3 + 500)

-- k <$> f <*> g = we will call k with results from f and g

-- (\x y z -> [x,y,z]) <$> (+3) <*> (*2) <*> (/2) $ 5  = [8.0,10.0,2.5]
-- We will call "create a list of the results of functions" with results from these three functions 
-- (each of which takes just one integer, so we only enter 5 -- entering a list of integers for functions to use would require mapping)