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
	fmap f g = (\x -> f (g x)) -- Mapping a function over another function! 

-- Another way to think of this: 
-- fmap :: (a -> b) -> ((->) r a) -> ((->) r b), or, in other words...
-- fmap :: (a -> b) -> (r -> a) -> (r -> b)  = we take a function that turns a -> b and a function that turns r -> a, produce a function that turns r -> b

-- So a much simpler version of the above:
-- instance Functor ((->) r) where
-- 	fmap = (.)

-- This is just function composition! We transform a variable in some way, then add another function that transforms it a second time based on the first result

-- fmap (*3) (+100) 1 = 303   -- Think of (+100) as a "box" that will eventually hold a result, then use (*3) to change what's in the "box"
-- fmap (show . (*3)) (+100) 1 = "303"