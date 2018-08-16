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


instance Applicative ZipList where
	pure x = ZipList (repeat x) -- will produce an infinite ZipList list, which is "minimal" because we need values in every possible position where a second list might need to be zipped 
	                            -- (which lets us satisfy the second, fmap law of functors)
	ZipList fs <*> ZipList xs = ZipList (zipWith (\f x -> f x) fs xs) -- apply each function in the first list to the corresponding value (not all values) in the second list)

-- We'll need getZipList here, since ZipList a doesn't have a "show" type
-- getZipList $ (+) <*> ZipList [1,2,3] <*> ZipList [100,100..] = [101,102,103] = map (+) to first list to make list of functions, zip it with the second list

-- getZipList $ (,,) <$> ZipList "dog" <*> ZipList "cat" <*> ZipList "rat" = [('d','c','r'),('o','a','a'),('g','t','t')]
-- the (,,) function in Haskell = \x y z -> (x,y,z) = how we easily grab item X from each of three lists (#QUESTION, not sure I fully understand this result, need to play with it later)
    -- We could use zipWith3, zipWith4, etc., but this is obviously more elegant (assuming we get more commas to handle that sort of thing, I suppose?)


-- Let's add more power to our use of functors!

liftA2 :: (Applicative f) => (a -> b -> c) -> f a -> f b -> f c -- take a function with two inputs, turn into a function that operates on two functors
liftA2 f a b = f <$> a <*> b -- apply the function to whatever is inside functors a and b

-- liftA2 (:) (Just 3) (Just [4])  -- functors within functors! (Lists within Maybes, in this case)

-- What if we wanted to add an arbitrary number of numbers from Maybe Ints to our Maybe list?
sequenceA :: (Applicative f) => [f a] -> f [a] -- start with functors inside a list, end with a functor that holds a list
sequenceA = foldr (liftA2 (:)) (pure []) -- the "pure" here lets us gather everything in a list (the parameter would be "[]" alone, but functors need a pure)

-- sequenceA [Just 3, Just 2, Just 1] = Just [3,2,1]
-- sequenceA [(+3),(+2),(+1)] 3 = [6,5,4] = glue each function to 3 with a <*> b, then list the results by folding in f = (:)
-- sequenceA [[1,2,3],[4,5]] = [[1,4],[1,5],[2,4],[2,5],[3,4],[3,5]] = see "we start off with" for a full explanation, we form lists [4] and [5] before gluing things to them 
    -- See "list comprehension" in the functors chapter for more explanation of this
    -- This kind of thing reminds us why lists count as functors (getting inside them to use functions wouldn't normally be this easy)
    -- To see the <*> definition that applies here, check "Applicative []"

-- sequenceA can help us with filtering
-- sequenceA [(>4),(<10)] 7 = [True,True] = again, folds our results into a list (from [a -> Bool] to a -> [Bool], we turn our list of functions into functions that can be applied to 7)

-- sequenceA [getLine,getLine,getLine] = staples all three lines into one list


-- More functor laws:

-- pure id <*> v = v
-- pure (.) <*> u <*> v <*> w = u <*> (v <*> w) = #QUESTION (how does the comma application work, exactly?)
-- pure f <*> pure x = pure (f x)
-- u <*> pure y = pure ($ y) <*> u = Applies pure to y, then applies y to u? #QUESTION, not certain I understand this


-- Newtype = take one type and present it as a different type (while data makes new types from scratch)
-- Unlike with data, you only get one field and one value constructor

newtype ZipList a = ZipList { getZipList :: [a] } -- lets us show the result of ZipList without needing to add getZipList every time
newtype CharList = CharList { getCharList :: [Char] } deriving (Eq, Show) -- takes a [Char], returns a CharList = CharList :: [Char] -> CharList

-- newtype is also helpful when we want to map over an instance of Functor in an unusual way (e.g. targeting first component of a tuple, not both)

newtype Pair a b = Pair { getPair :: (a,b) } -- The book had Pair b a, not a b. Why is that? #QUESTION
instance Functor (Pair c) where
	fmap f (Pair (x,y)) = Pair (f x, y) -- There! Now our function only touches the first element of our pair
	-- We needed newtype to transform our pair of values into a single value constructor (so that Functor would accept it)


-- Advantage of newtype > data: Haskell doesn't need to check which value constructor we're using, 
    -- so there are fewer ways to hit exceptions (e.g. if we pass in "undefined" somewhere)


-- Type vs. Newtype vs. Data

-- type = give an existing type a new name for ease of reference
type IntList = [Int]
-- ([1,2,3] :: IntList) ++ ([4,5] :: [Int]) = [1,2,3,4,5]
type PhoneBook = [(String,String)]

-- newtype = take an existing type and wrap it in a new type (to help it fit into a typeclass like Functor) (like data, but more efficient, and only for one-to-one)
newtype CharList = CharList { getCharList :: [Char] } -- CharList isn't a list, but does "contain" a list (and can be converted to a list with getCharList)
newtype Real = MakeReal Integer -- we have a new type (Real) with a constructor that converts an Integer to that type

toReal :: Integer -> Real
toReal x = MakeReal x

fromReal :: Real -> Integer
fromReal (MakeReal x) = x

-- data = make a type with as many constructors and fields as you want (see making_types.hs)



-- Monoids! These are a combination of an associative binary function (*, ++) and an identity value (1, [])

class Monoid m where -- concrete types only, no parameters here (so no type constructors, unlike Functor or Applicative)
	mempty :: m -- the identity value
	mappend :: m -> m -> m -- the binary function, which takes two values and returns one value of the same type (* with two numbers returns one number)
	mconcat :: [m] -> m -- appends all the monoid values in a list together, returning one value 
	mconcat = foldr mappend mempty -- this is a little redundant, but sometimes we might be able to do it more efficiently, so this is a reminder to check
	    -- (but we will get mconcat by default when we define a new monoid, since it uses the mempty and mappend definitions we give i)t

-- Monoid laws:
-- mappend mempty x = x    (identity value!)
-- mappend x mempry = x    (identity value! Again!)
-- mappend (mappend x y) z = mappend x (mappend y z)    (associative property!)   (but monoids needn't be commutative -- lists certainly aren't)

instance Monoid [a] where  -- [a], not [], we need a concrete type   (#QUESTION: Review the definition of "concrete type")
	mempty = []
	mappend = (++)

-- the number type can be an instance of the Monoid class in two ways (addition and multiplication)
-- Data.Monoid gives us Sum and Product types

newtype Product a = Product { getProduct :: a } -- getProduct just pulls the number a out of Product a: getProduct (Product 9) = 9
    deriving (Eq, Ord, Read, Show, Bounded)

instance Num a => Monoid (Product a) where
	mempty = Product 1 -- just the number 1 "wrapped in a Product constructor"
	mappend Product x Product y = Product (x * y)

newType Sum a = Sum { getSum :: a }
    deriving (Eq, Ord, Read, Show, Bounded)

instance Num a => Monoid (Sum a) where
	mempty = Sum 0
	mappend Sum a Sum b = Sum (a + b)

-- Bools can be monoids in two ways: False = mempty and || = mappend, or True = mempty and && = mappend

-- You can even do this with the Ordering type!

instance Monoid Ordering where
	mempty = EQ
	mappend LT _ = LT -- LT _ is always LT, if _ is LT, EQ, or GT
	mappend EQ y = y  -- If we're deciding which of two things is "greater", we can ignore equal elements and move on to the next comparison (adding an equal element doesn't change the overall comparison)
	mappend GT _ = GT -- GT _ is always GT, if _ is LT, EQ, or GT

lengthCompare :: String -> String -> Ordering -- compare lengths of two strings, or compare them alphabetically if they are of equal length
lengthCompare x y = mappend (compare length x length y) -- if X or Y is of greater length, put that one first
                            (compare x y) -- if X and Y are the same length, choose the one starting with the later letter of the alphabet

-- The function above is much more efficient than what you could get without monoids (you create fewer variables to store values)
-- If we wanted to add additional criteria for comparison, we could just throw more "compare" statements into our series of mappends, and we'd stop wherever we hit a non-EQ


-- Maybe can also be a monoid by making its type parameter a monoid

instance Monoid a => Monoid (Maybe a) where -- a must be an instance of monoid for this to make sense
	mempty = Nothing
	mappend Nothing m = m
	mappend m Nothing = m
	mappend Just m1 Just m2 = Just (mappend m1 m2) -- monoid rules will apply for m1 and m2

-- Or we can build a version that works for non-monoid types:

newtype First a = First { getFirst :: Maybe a }
    deriving (Eq, Ord, Read, Show)

instance Monoid (First a) where
	mempty = First Nothing
	mappend First (Just x) _ = First (Just x) -- return the first parameter when we map, unless that parameter is Nothing
	mappend First Nothing x = x -- If we mconcat a cluster of Maybes, we'll retain the first non-nothing (thanks to the rule above)

-- This helps us learn if any of our Maybes is non-Nothing through mconcat
-- There's also a Last a type in Data.Monoid, which gives back the *last* non-Nothing value when we mappend and mconcat


-- Monoids are very handy for folding!
import qualified Foldable as F -- fives us functions that can fold anything, not just lists
-- F.foldl (+) 2 (Just 9) = 11
-- F.foldr (||) False (Just True) = True

-- We also get this bad boy:
foldMap :: (Monoid m, Foldable t) => (a -> m) -> t a -> m  -- take a function that returns a monoid and a foldable structure, return a monoid value

instance F.Foldable Tree where -- choose a function to fold over your tree (e.g. multiplying a tree full of numbers)
	foldMap f Empty = mempty -- folding in an empty node in the tree shouldn't alter the final result
	foldMap f (Node x l r) = F.foldMap f l `mappend`
	                         f x           `mappend`
	                         F.foldMap f r -- fold up everything under our node along with the node value (x)

-- We can use this to search our tree!
-- getAny $ F.foldMap (\x -> Any $ x == 3) testTree  
    -- = use a function that runs through our tree, replacing each branch x with Any True (if it matches 3) or Any False (if it doesn't), 
    -- then mappend the results (True if you have at least one Any True, False otherwise)

-- And foldMap (\x -> [x]) turns our tree (or other foldable thing) into a list!


-- In the end, we've built up a toolbox with lots of ways to tackle any given problem, because we keep breaking ideas down into simple structures