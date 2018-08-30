-- Monads = "beefed-up applicative functors"

-- How we got here:

-- We were turning types into other types within data types (functors)
-- And we were even turning functions into other types of functions (more functors)

-- We had fmap :: (Functor f) => (a -> b) -> f a -> f b 
-- And by writing the correct Functor, we could make fmap change anything

-- Then we went farther by building:
-- (<*>) :: (Applicative f) => f (a -> b) -> f a -> f b
-- And then we could apply not only functions, but functions *inside our data types*

-- (*) <$> Just 2 <*> Just 8    = Just 16
-- (-) <$> [3,4] <*> [1,2,3]    = [2,1,0,3,2,1]

-- We extended this by turning values into values with contexts, through monads:
-- (>>=) :: (Monad m) => m a -> (a -> m b) -> m b    = bind, a way to feed "fancy values" into functions

-- Version of "bind" just for Maybe:

applyMaybe :: Maybe a -> (a -> Maybe b) -> Maybe b
applyMaybe Nothing f  = Nothing  -- Accomodate the fail case!
applyMaybe (Just x) f = f x   -- Remember that our function must take x and return a Maybe

-- This is a tiny example of the Monad type class:

class Monad m where -- every monad is applicative, but they were invented too long ago to need class (Applicative m) in front
	return :: a -> m a  -- same as "pure" for the Applicative class (wraps anything in our chosen context -- the monad context)

	(>>=) :: m a -> (a -> m b) -> m b

	(>>) :: m a -> m b -> m b -- default function we'll rarely add to a Monad instance (but why is it here? #QUESTION)
	x >> y = x >>= \_ -> y

	fail :: String -> m a
	fail msg = error msg -- This will be relevant later


instance Monad Maybe where
	return x = Just x
	Nothing >>= f = Nothing
	Just x >>= f  = f x -- extracting information from a Just -- is that all? (No pattern-matching required, though, so that's nice)
	fail _ = Nothing

-- Just 9 >>= \x -> return (x*10) = Just 90    -- so we actually return Just 90, not 90 -- doesn't seem quite like what the instance would imply (#QUESTION)
                                               -- ...maybe we're actually returning a Just automatically based on the type of the function?

type Birds = Int 
type Pole = (Birds,Birds) -- we'll need to keep both sides of this "pole" balanced

landLeft :: Birds -> Pole -> Maybe Pole -- using a "Maybe Pole" lets us break off our function into a Nothing if too many birds land on one side of the pole
landLeft n (left,right) = (left + n,right)
    | abs ((left + n) - right) < 4 = Just (left + n, right)
    | otherwise                    = Nothing

landRight :: Birds -> Pole -> Maybe Pole
landRight n (left,right) = (left,right + n)
    | abs ((left + n) - right) < 4 = Just (left, right + n)
    | otherwise                    = Nothing

-- landLeft 2 (landRight 1 (0,0)) = (2,1) -- this would work if we returned a Pole, but not when we return a Maybe Pole 
	-- and... starting with a Maybe Pole would be inconvenient? 
	-- Well, monads seem to make things simpler

-- return (0,0) >>= landRight 2 >>= landLeft 2    -- Just (2,4)   With monads, we can keep throwing integers at Maybes and getting results!
-- return (0,0) >>= landRight 2 >>= landRight 2 >>= landLeft 2    -- Returns Nothing, because the pole was overbalanced at one point
-- if we add   >> Nothing   to the chain somewhere, we'll always get Nothing. >> lets us return monad values of our choice (at least those defined as special cases in our instance)

-- Not having a >> Nothing option (a "failure context") would not be pretty:

routine :: Maybe Pole  
routine = case landLeft 1 (0,0) of -- I need to review case functionality in Haskell to better interpret this (#QUESTION)
    Nothing -> Nothing  
    Just pole1 -> case landRight 4 pole1 of   
        Nothing -> Nothing  
        Just pole2 -> case landLeft 2 pole2 of  
            Nothing -> Nothing  
            Just pole3 -> landLeft 1 pole3  -- ...and so on, for every landing, since we lack a simple way to handle a Nothing at some point in the chain
                                            -- (this is all nice, but I hope we get a really strong speed upgrade from Python as a result...)

-- Anyway, that's one good use for monads: supporting computations that can "fail" or hit another default outcome somewhere in the middle.


-- We can use "do" for any monad, not just I/O

foo :: Maybe String
foo = do
	x <- Just 3 -- binds Just 3 to a variable as a string ("3")
	y <- Just "!" -- binds Just ! to a variable as a string ("!")
	Just (show x ++ y) 
	-- without do notation, we'd have to use a chain of >>= to achieve this:
	    -- Just 3 >>= (\x -> Just "!" >>= (\y -> Just (show x ++ y)))
	-- The new result: We can extract things from Maybe values without checking for Just/Nothing at every step (if we throw a Nothing into Foo, we'll get Nothing)

-- Back to the pole routine:

routine :: Maybe Pole
routine = do
	start <- return (0,0) -- do we need the "return" to ensure we return something at the end? (#QUESTION)
	first <- landLeft 2 start
	-- Nothing    -- we can throw this in anywhere and get "Nothing" at the end (equivalent to _ <- Nothing)
	second <- landRight 2 first
	landLeft 1 second -- We still return a "Just" value here, of course

-- Even if this all looks imperative, it remains sequential -- each line relies on the result of the previous lines (and their non-failure)

-- Note: Since all the results so directly rely on the last result, >>= might actually be better than do notation here (but do notation is more flexible)


-- We call our monad "fail" function (better than crashing!) when we use "do" notation and run to the end of our pattern matching without success


-- How monads work for lists:
instance Monad [] where
	return x = [x]
	xs >>= f = concat (map f xs) -- "concat" pulls together the set of results from the map (which might otherwise look like a list of lists)
	fail _ = []

-- An empty list anywhere will return an empty list, since you can't map over it or map "turn things empty" and get a result back

-- [1,2] >>= \n -> ['a','b'] >>= \ch -> return (n,ch)   = we bind the lists to n and ch, respectively, then bind each possible (n,ch) tuple
    -- Note that we've bound several lists together here, and keep going until we hit a "return"
    -- The "return" creates a minimal context for (n,ch), which is exactly one instance of each possible tuple 
         -- (this is non-deterministic, where choosing certain tuples would actually determine the outcome)
    -- #QUESTION: Make sure you can map this out! It's a bit recursive in feel, trace where all the variables end up

listOfTuples :: [(Int,Char)] -- Return a list of tuples, each of which is in (Int,Char) format
listOfTuples = do
	n <- [1,2]
	ch <- ['a','b']
	return (n,ch) -- We've created something more like a list comprehension, and do notation translates to >>= for us

-- Given that we have list comprehension-like functionality, let's replicate filtering in monads:

class Monad m => MonadPlus m where
	mzero :: m a
	mplus :: m a -> m a -> m a

guard :: (MonadPlus m) => Bool -> m ()
guard True = return () -- if true, return a minimal context (e.g. [()] for a list)
guard False = mzero

-- guard (4 > 1) >> return "true" :: [String]   = ["true"] = now we have a way to not execute certain computations in the middle of a monad!

instance MonadPlus [] where
	mzero = []
	mplus = (++)

-- [1..50] >>= (\x -> guard (elem '7' show x) >> return x)  = [7,17,27,37,47]
    -- Mapping over [1..50], we only return [(7)]... and so on, otherwise returning [], then concatenate all of those returns

sevensOnly :: [Int]
sevensOnly = do
	x <- [1..50]
	guard (elem '7' show x) -- In do notation, "guard" is just a filter, giving us back results that fit and "zeroing" results that don't
	return x


-- Monad laws:

-- 1. return x >>= f   MUST BE THE SAME THING AS   f x   (that's the definition of putting the value in a "minimal" context -- we shouldn't change the way f sees it)
-- 2. m >>= return   IS THE SAME THING AS   m  (where m is any monadic value)
  -- >>= just produces a map of our function over the value, and mapping "return" produces a list of values that can be concatenated to recreate the original value
  -- monadic left/right identity laws are just describing how "return" should behave

-- 3. Associativity law of monads: any nesting shold work
  -- (m >>= f) g   IS THE SAME AS    m >>= (\x -> f x >>= g)  = two ways of saying "feed value m to function f, then feed the result to function g"

  -- For example:
  -- return (0,0) >>= landRight 2 >>= landLeft 2 >>= landRight 2

  -- Gives the same result as:
  -- return (0,0) >>= (\x ->    -- With >>=, x becomes (0,0)
  -- landRight 2 x >>= (\y ->   -- This function landRights 2 on (0,0) and feeds (0,2) to become y through >>=
  -- landLeft 2 y >>= (\z ->    
  -- landRight 2 z)))

-- Thanks to this, we can use composition on monadic functions:

(<=<) :: (Monad m) => (b -> m c) -> (a -> m b) -> (a -> m c)
f <=< g = (\x -> g x >>= f) -- Same as g (f x), since x >>= f feeds x into f

let f x = [x,-x]
let g x = [x*3,x*2]
let h = f <=< g -- feed the result of g into f, which means running f on g, which means producing a maximally nondeterministic list
h 3   -- [9,-9,6,-6]

-- return <=< f   IS THE SAME AS   f <=< return   IS THE SAME AS   f
-- ...which means that return with <=< works the same as id with .


-- IN SHORT: >>= and do notation let us "focus on the values themselves while the context gets handled for us"
    -- For example, by letting us bring in   Maybe a   to a function that just wants   a -> Maybe b
-- EVERY MONAD HAS ITS OWN USES: The Maybe monad lets us fail safely halfway through a function, 
    -- and the List monad lets us work with lists of nondeterminate outcomes (see ex/knight_moves.hs)

-- And now, Writer, which lets us add "log values" to computations and produce an extra "log result"

applyLog :: (Monoid m) => (a,m) -> (a -> (b,m)) -> (b,m) -- the monoids attached here serve as logs
applyLog (x,log) f = let (y,newLog) = f x in (y, log `mappend` newLog) -- Remember that "mappend" combines any two monoids the way ++ combines strings
  
    -- For example, we could now create a store with (String,Int) tuples representing prices, and add up a total price as we went along
    -- Writer is the best way to do this:

newtype Writer w a = Writer { runWriter :: (a,w) } -- newtype lets us make Writer an instance of Monad

instance (Monoid w) => Monad (Writer w) where
	return x = Writer (x, mempty)
	(Writer (x,v)) >>= f = let (Writer (y,v')) = f x in Writer (y, v `mappend` v')
    -- Applying x to f gives us a   Writer w a   value, and we pattern-match that with a "let"
    -- We set y equal to f x    and v' is coming from f, I think, based on how isBigGang worked in "A Few Monads More" (#QUESTION, make sure this is true)

-- Using Writer with do notation

logNumber :: Int -> Writer [String] Int  -- Because of how Writer is built as a newtype, we enter [String] and Int in reverse order (#QUESTION: Why is this necessary?)
logNumber x = Writer (x, ["Got number: " ++ show x])

multWithLog :: Int -> Int -> Writer [String] Int
multWithLog x y = do
	a <- logNumber x
	b <- logNumber y
	tell ["Multiplying both numbers"] -- part of MonadWriter type class, creates Writer ((),String) so that we add to the log without binding any variable (() remains unbound)
	return (x*y)

-- runWriter multWithLog 3 5   = (15,["Got number: 3","Got number: 5","Multiplying both numbers"])

-- An example with a recurring function:

gcd' :: Int -> Int -> Writer [String] Int
gcd' a b
    | b == 0 = do   -- This could also have been   Writer (a, ["Finished with " ++ show a]), but separating the note and the return is very readable!
    	  tell ["Finished with " ++ show a] -- We found the GCD!
    	  return a
    | otherwise = do
    	  tell [show a ++ " mod " ++ show b ++ " = " show (a `mod` b)] -- Leave giant note showing our full computation
    	  gcd' b (a `mod` b) -- This works as a line in our "do" expression because gcd' will eventually return a Writer value, which is a real monad

-- mapM_ putStrLn $ snd $ runWriter (gcd' 8 3)  
-- 8 mod 3 = 2  
-- 3 mod 2 = 1  
-- 2 mod 1 = 0  
-- Finished with 1  

-- See "A Few Monads More" for a warning against functions that associate "to the left instead of the right"
  -- gcd' could be written to stack up a collection of pending logs rather than producing each log as we go, which would be very inefficient


-- Up next: Difference lists
-- These are lists, but with an automatic append attached to them
    -- Why do this? Lets us immediately stick something to the back of our left-side list without building a new list from scratch
    -- (because we don't have to "walk all the way to the end" of that list to attach the other one)

f = ("dog"++)
g = ("hair"++)
f `append` g -- "doghair"++

newtype DiffList a = DiffList { getDiffList :: [a] -> [a] } -- Difference lists are functions that take one list and return another

toDiffList :: [a] -> DiffList a
toDiffList xs = DiffList (xs++) -- return a DiffList type that lets us auto-append to the end of our list

fromDiffList :: DiffList a => [a]
fromDiffList (DiffList f) = f [] -- Since f is xs++, xs++ [] just creates [xs]

instance Monoid (DiffList a) where
	mempty = DiffList (\xs -> [] ++ xs) -- I don't think we can actually just write []++ here  (this is just the id function)
	(DiffList f) `mappend` (DiffList g) = DiffList (\xs -> f (g xs)) -- paste f and g together, creating a function where you can paste xs on the end

-- If we wanted, we could use this to improve something like the slow reverse gcd' function 
    -- (building the list more easily thanks to lazy properties, I think? #QUESTION on how this looks step-by-step)
    -- #QUESTION: Check speed differences between the two versions of gcdreverse


-- We can also turn all functions into monads (!)

instance Monad ((->) r) where
	return x = \_ => x -- Returns a function where anything you enter returns x
	h >>= f = \w -> f (h w) w -- Returns a function where, if we enter w, we get the composition of h and f on w (just like we saw with functors and <*>)

-- in "do" notation:

addStuff :: Int -> Int
addStuff = do
	a <- (*2)
	b <- (+10)
	return a + b -- Both a and b are functions, so this works out the same as   (+) $ (*2) <*> (+10)
	  -- All "return" does here is present a monadic value as a result, just like anywhere else
    
-- This setup is great for applying a bunch of functions to one thing that we want to throw in later (the sort of thing imperative languages do well!)


-- Speaking of imperative programming, here's another tool we can use to make imperative-style work less tedious: The state monad

-- "Stateful computation" = function that takes a state, returns a value and a state (as with our random-number generators in an earlier chapter)

type Stack = [Int]

pop :: Stack -> (Int,Stack)
pop (x:xs) -> (x,xs)

push :: Int -> Stack -> ((),Stack)
push a xs = ((),a:xs) -- a:xs is the state we have after, but we aren't "getting" anything (just putting something on the stack), so we return "nothing"

stackManip :: Stack -> (Int, Stack)
stackManip = do  -- This is MUCH easier than manipulating states manually and producing lots of variables to represent those states (see other StackManip)
	push 3 -- Put 3 on our stack
	a <- pop -- Get 3 off our stack
	pop -- Get the top number off our 


newtype State s a = State { runState :: s -> (a,s) } -- #QUESTION: Review why this is the syntax, and what exactly "runState" will do

instance Monad (State s) where
	return x = State $ \s -> (x,s) -- return x with the original state (minimal context)
	(State h) >>= f = State $ \s -> let (a, newState) = h s -- produce a new state using our old stateful computation (h) and our current state (s)
	                                                        -- #QUESTION: What, exactly, is "a" doing here? Is it holding a value?
	                                    (State g) = f a -- produce a new stateful computation using our function 
	                                in  g newState -- result: a tuple of a result (g) and a state (newState)   

-- This lets us rewrite pop and push with the State type:

-- We are implicitly operating on a list with this formatting, no use of xs as an input to these

pop :: State Stack Int --> Produces a state containing a Stack and an Int, is a stateful computation
pop = State $ \(x:xs) -> (x,xs)

push :: Int -> State Stack ()
push a = State $ \xs -> ((),a:xs)

stackStuff :: State Stack () -- We can now easily produce imperative-style "stateful computations"
                             -- We can even chain them together as long as we always have an existing "state" to work with
stackStuff = do
	a <- pop
	if a == 5
		then push 5
		else do
			push 3
			push 8

-- Default functions for the state monad: get (returns current state) and put (replaces a state with a newState by force)
-- If >>= worked only for state values, it would be the type:   (>>=) :: State s a -> (a -> State s b) -> State s b   = state type remains, result type might change
    -- Take a state and a function that turns a state value into a new state, get a new state


-- Putting this all together, here's how we build randomness:

random :: (RandomGen g, Random a) => g -> (a,g) -- Start with a generator, return a generated value and new generator (a stateful tuple!)

randomSt :: (RandomGen g, Random a) => State g a
randomSt = State random

threeCoins :: State StdGen (Bool,Bool,Bool) -- #QUESTION: Go check the first "threecoins" function, check how much smoother it's become
threeCoins = do
	a <- randomSt -- All the new generator production is now tastefully hidden
	b <- randomSt
	c <- randomSt
	return (a,b,c)


-- Either, as an "enhanced Maybe", is also a monad:

instance (Error e) => Monad (Either e) where -- keep errors on the left, returns on the right
	return x = Right x
	Right x >>= f = f x -- You need to return Either String a  in your functions, where "a" is the type of the Right return
	Left err >>= f = Left err -- anytime we feed in the Left/error value, we return it with no regard for x
	fail msg = Left (strMsg msg) -- strMsg creates an exception with the msg we added

-- #QUESTION: A recommended exercise is to rewrite the pole-walker example with an error message that returns the bird distribution after he falls


-- Here's a collection of monadic functions! (Remember: Monads feed values with context into normal functions and return values with context)

liftM :: (Monad m) => (a -> b) -> m a -> m b -- fmap for monads! We never need a Functor, which shows us how much more flexible monads are
liftM f m = m >>= (\x -> return (f x)) -- we simply apply f to m, whatever form m takes, because >>= lets us apply something over an entire monad already

liftM :: (Monad m) => (a -> b) -> m a -> m b -- we can also skip >>= through do notation
liftM f m = do 
	x <- m 
	return (f x) -- we apply f to m, then make sure we're back in a default context with "return"  
	     -- #QUESTION: Dissect an example of this down to the roots, make sure you understand the type of each value at each step

-- liftM (*3) Just 8  -- Just 24
-- runWriter $ liftM not $ Writer (True, "abc")  -- (False, "abc")
-- runState (liftM (+100) pop) [1,2,3,4]  -- (101,[2,3,4])

ap :: Monad m => m (a -> b) -> m a -> m b  -- This is <*> for monads, letting us take a function inside a monad value and apply that function to another monad value
ap mf m = do
	f <- mf -- extracts the function, using the <- from do notation (which always grabs the inner value from a monad context)
	x <- m
	return (f x)

-- Just (+3) `ap` Just 4  -- Just 7
-- [(+1),(+2),(+3)] `ap` [10,11]  -- [11,12,12,13,13,14]   -- This works because of the way we set up the list monad long ago (to apply all functions in a list!)

-- Again, monads look "strong" (flexible) here, because we can replicate Applicative just using Monad's functions
-- In real Haskell programming, people often make Applicative instances from Monad instances by setting "pure" as return and <*> as ap
    -- Or make Functors from Monads by setting fmap to liftM

-- Finally, liftM2 is the monad version of liftA2 (it lets us apply a function between two applicative values, producing a third function -- see earlier chapter)


-- Oooh! We can flatten monads, too!

join :: (Monad m) => m (m a) -> m a
join mm = do
	m <- mm  -- Get the result of mm, which will be a monadic value in and of itself (all context is taken care of)
	m

-- #QUESTION: THIS ONE IS IMPORTANT! Go back to do notation and figure out how <- grabs the result 

-- join (Just (Just 9))  -- Just 9
-- join (Right (Right 9))  -- Right 9
-- join [[1,2],[3,4]]  -- [1,2,3,4]   -- the general-purpose "join" function turns into concat when applied to a list...

-- ...and mappend when applied to a nested Writer value

-- runWriter $ join (Writer (Writer (1,"a"),"b"))  -- (1,"ba")  (we get our value out alone, but join the logs)


-- For every monad, join (fmap f m) is the same thing as >>=
	  -- This means we can use the join/map combination to figure out how to implement >>= if that's tricky for a new monad


-- And we also have filtering:
filterM :: (Monad m) => (a -> m Bool) -> [a] -> m [a] -- We return m [a] because we want to have the context of our monad
filterM f = foldr (\x -> liftA2 (\y -> if y then (x:) else id) (f x)) (pure []) -- f x returns True or False, and that becomes the value of y
    -- prepend x to our list if our filter returns true; if not, we just return the id value (which is our accumulator value -- in this case, pure)

    -- I found this explanation of filterM helpful: https://blog.ssanj.net/posts/2018-04-10-how-does-filterm-work-in-haskell.html

keepSmall :: Int -> Writer [String] Bool -- This gives us a log of everything we filter in/out, and why
keepSmall x 
    | x < 4 = do
    	  tell ["Keeping " ++ show x]
    	  return True
    | otherwise = do
    	  tell [show x ++ " was too large"]
    	  return False

-- Now, to actually RETRIEVE the log, we need mapM_ putStrLn

-- mapM_ putStrLn $ snd $ runWriter $ filterM keepSmall [9,1,5,2,10,3]   (we use snd and not fst because we only want the logs, not the filtered list [1,2,3])
-- 9 is too large, throwing it away  
-- Keeping 1  
-- 5 is too large, throwing it away  
-- Keeping 2  
-- 10 is too large, throwing it away  
-- Keeping 3  


-- Getting all subsets of a monad set:

powerset :: [a] -> [[a]] -- Return a list of lists
powerset xs = filterM (\x -> [True, False]) xs -- We both drop and keep every element of the list
    -- By doing this, we get lists like [True,True,False] [1,2], [False,True,True] [2,3], and [False,False,False] []


-- From filters, we'll move to folds:
foldM :: (Monad m) => (a -> b -> m a) -> a -> [b] -> m a -- our input function returns a monadic value, so our result is monadic

binSmalls :: Int -> Int -> Maybe Int
binSmalls acc x
    | x > 9     = Nothing -- Make sure we only add small numbers, and fail if we hit a big one
    	    -- This do structure also makes it easy to use Writer with folds
    | otherwise = Just (acc + x)

-- foldM binSmalls 0 [2,3,4]  = 9
-- foldM binSmalls 0 [2,3,10]  = Nothing  


-- Let's take our old RPN calculator and safety-proof it, so that we won't crash on any error

readMaybe :: (Read a) => String -> Maybe a
readMaybe st = case reads st of [(x,"")] -> Just x -- If we read something, it must be the data type we're looking for
                                _ -> Nothing -- If we don't get [(x,"")] by reading (that is, we return an error), send back a Nothing

foldingFunction :: [Double] -> String -> Maybe [Double] -- Sometimes, we'll want to fail, and access to Nothing makes that easy
foldingFunction (x:y:ys) "*" = return ((x * y):ys)  
foldingFunction (x:y:ys) "+" = return ((x + y):ys)  
foldingFunction (x:y:ys) "-" = return ((y - x):ys)  
foldingFunction xs numberString = liftM (:xs) (readMaybe numberString) -- If we've hit a non-Int, we now return Nothing

solveRPN :: String -> Maybe Double
solveRPN st = do
	[result] <- foldM foldingFunction [] (words st) -- replaced foldl with foldM to return a Maybe value rather than just an Int or error
	return result                                   -- if foldM sees more than one number in the stack, the RPN was ambiguous and we'll still return Nothing 
		                                                  -- #QUESTION: Try this out and make sure you can figure out exactly why/when two numbers in the stack will fail

-- Composing monadic functions: Just use <=<
-- But you can also compose an *infinite* number of monadic functions this way!

-- Let's have our knight from earlier move an arbitrary number of times:

inMany :: Int -> KnightPos -> [KnightPos]
inMany x start = return start >>= foldr (<=<) return (replicate x moveKnight) -- Basically, we generate a list of "moveKnight" functions, then glue them together with <=<

canReachIn :: Int -> KnightPos -> KnightPos -> Bool
canReachIn x start end = end `elem` inMany x start -- No need to hardcode the number "3" anymore!


-- Making a new monad type: Lists of numbers with associated probabilities (handy!)

newtype Prob a = Prob { getProb :: [(a,Rational)] } deriving Show 
instance Functor Prob where
	fmap f (Prob xs) = Prob $ map (\(x,p) -> (f x,p)) xs   -- Map over our Ints, leave the probabilities unchanged

-- Easy to see that our minimal context is (x,1), since probabilities always add to 1
-- As for >>=, remember that it's always equal to join (fmap f m)
    -- If we want to flatten sub-probabilities, we need to multiply them together (1/2 chance of a 2/3 chance = 1/3 chance)

flatten :: Prob (Prob a) -> Prob a
flatten (Prob xs) = Prob $ concat $ map multAll xs
    where multAll (Prob innerxs,p) = map (\(x,r) -> (x, p*r)) innerxs 
    -- We have a probability of each inner set of probabilities, but the only x-value "outcomes" are in the nested level
    -- So for each inner set of probabilities, we multiply by the corresponding p-value in our list
    -- #QUESTION: When you come back, set up an example of this

instance Monad Prob where
	return x = Prob [(x,1%1)] -- We write probabilities as rational numbers, not floating-points (easier to avoid bugs)
	m >>= f = flatten (fmap f m)
	fail _ = Prob [] -- The "Nothing" of probabilities

-- return x >>= f will equal f x, since we just multiply all probabilities by 1
-- m >>= return holds by the same logic
-- And of course, multiplication is associative, so f <=< (g <=< h) should equal (f <=< g) <=< h

data Coin = H | T deriving (Show, Eq)

coin :: Prob Coin
coin = Prob [(H,1%2),(T,1%2)]

loadedCoin :: Prob Coin
loadedCoin = Prob [(H,1%10),(T,9%10)]

flipThree = Prob Bool
flipThree = do
	a <- coin
	b <- coin
	c <- loadedCoin
	return (all (==Tails) [a,b,c]) -- Will return seven "False" probabilities for outcomes with any heads and one "True" probability for all-tails outcomes