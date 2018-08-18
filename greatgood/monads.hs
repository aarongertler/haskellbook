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

	(>>) :: m a -> m b -> m b -- default function we'll rarely add to a Monad instance (but why is it here? #!UESTION)
	x >> y = x >>= \_ -> y

	fail :: String -> m a
	fail msg = error msg