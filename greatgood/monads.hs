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