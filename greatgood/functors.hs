-- Functors and Applicative Functors

-- Open typeclaases and a wide range of types = high-level polymorphism! 
-- We can define typeclasses for very general/abstract behavior

-- As a reminder, functors are things we can map over (lists, maybes, trees, etc.)
-- That's where we get the fmap function
-- Also, remember that any type constructor we make into an instance of Functor is * -> * (e.g. Maybe works, because String -> Maybe String)
-- And to use something like Either, we can only make (Either a) into a functor, ignoring its other half
-- fmap :: (B -> c) -> Either a b -> Either a c

-- Another functor type: IO (is * -> *, because we can have IO String for grabbing strings with IO actions)

instance Functor IO where
	fmap f action = do -- for example, we could fmap "words" onto "getline", get a line, then split it into words
		result <- action
		return (f result) -- returns an IO action (result), so we know we need "do" syntax

main = do line <- fmap reverse getline
    putStrLn $ "You said " ++ line ++ " backwards!"

-- Why do we need the fmap for this? Could some other function work just as well? #QUESTION