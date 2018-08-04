-- Find the shortest path when you can take one of two paths (or cross between them)

-- Is it worth building a custom data type? We could try this with a list...

shortestPath :: [Num] -> [String]


-- Thankfully, this is a simpler version of several Project Euler problems.
-- Just keep track of two paths, and see what's shorter at the end (checking each step along the way)

-- CHECK YOUR EULER METHODS BEFORE YOU KEEP GOING!