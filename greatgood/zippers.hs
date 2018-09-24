-- We've built data structures; now, we learn how to change them

data Tree a = Empty | Node a (Tree a) (Tree a) deriving (Show)

-- Trying to hunt down a particular element involves a lot of clumsy pattern-matching
-- Instead, we can do this:

data Direction = L | R deriving (Show)
type Directions = [Direction]

changeLetter :: Directions -> Tree Char -> Tree Char -- Take a list of directions, then travel down the tree, getting back the nwzr nosw each time...
changeLetter (L:ds) (Node x l r) = Node x (changeLetter ds 1) r
changeLetter (R:ds) (Node x l r) = Node x l (changeLetter ds r)
changeLetter [] (Node _ l r) = Node 'P' l r -- ...until you reach the node at the end of your directions and change it
  -- We're still setting a node to a specific character at this point

-- Getting the element at our destination:

elemAt :: Directions -> Tree a -> a -- Grab an element after submitting a list of left/right moves
elemAt (L:ds) (Node _ l _) = elemAt ds l -- If current first direction in the list is "left"...
elemAt (R:ds) (Node _ _ r) = elemAt ds r
elemAt [] (Node x _ _) = x  -- Stop and give your value once you reach the end of the directions

-- It would be great if we could zoom in on nearby subtrees without taking it from the top each time. Thankfully, we can!

-- type Breadcrumbs = [Direction] -- We'll remember which way we traveled at each point

-- goLeft :: (Tree a, Breadcrumbs) -> (Tree a, Breadcrumbs)
-- goLeft (Node _ l _, bs) = (l, L:bs) -- Add an L breadcrumb when we move left, bs is the rest of our Breadcrumb list (why? #QUESTION)

-- goRight :: (Tree a, Breadcrumbs) -> (Tree a, Breadcrumbs) -- One step complete -- we now know that we came from the left or right to get to the current node
-- goRight (Node _ r _, bs) = (r, R:bs)

-- Now if we chain something like (goLeft (goRight (Tree, []))), we'll get a set of notes but also [L,R] breadcrumbs at the end 
  -- For better syntax, just use (Tree, []) -: goRight -: goLeft

data Crumb a = LeftCrumb a (Tree a) | RightCrumb a (Tree a) deriving (Show) 
  -- Each "crumb" contains both the element we moved from and the element we *didn't* visit (all we don't have is the element we did visit, since we are there anyway)

type Breadcrumbs a = [Crumb a] -- We'll remember which way we traveled at each point

goLeft :: (Tree a, Breadcrumbs) -> (Tree a, Breadcrumbs)
goLeft (Node x l r, bs) = (l, LeftCrumb x r:bs) -- Now we grab all the info from x and r, so we include them as well

goRight :: (Tree a, Breadcrumbs) -> (Tree a, Breadcrumbs) -- One step complete -- we now know that we came from the left or right to get to the current node
goRight (Node x l r, bs) = (r, RightCrumb x l:bs) -- This does still error out on an empty node -- something to fix later

goUp :: (Tree a, Breadcrumbs a) -> (Tree a, Breadcrumbs a)
goUp (t, LeftCrumb x r:bs) = (Node x t r, bs)   -- "t" is our tree, so we create a new sub-tree where our tree was "left" and now we've jumped up to x,
goUp (t, RightCrumb x l:bs) = (Node x l t, bs)       -- which has a left sub-tree (t) and also a right sub-tree (r)
  -- Oh, and we pick up our breadcrumbs as we go along, so that our "collection" reflects the minimum path to have reached our position after going up
  -- Later, we'll use Maybe to fix the problems we'd face trying to go up from the top of a tree


type Zipper a = (Tree a, Breadcrumbs a) -- This pair fully describes a certain piece of any tree (the piece we record as we set out a particular trail of crumbs)

modify :: (a -> a) -> Zipper a -> Zipper a
modify f (Node x l r, bs) = (Node (f x) l r, bs) -- Run a function over the values of our nodes! (for any node we happen to be focusing on) (not editing the branches below)
modify f (Empty, bs) = (Empty, bs)

-- let newFocus = (freeTree,[]) -: goLeft -: goRight -: modify (\_ -> 'P')   -> go left, then right, the turn the node you find into a P (no matter what it was before)
    -- This is a "replacing" function -- you could also multiply the node, subtract from it, etc.

-- let newFocus2 = modify (\_ -> 'X') (goUp newFocus)   -> take the above modified tree, go up a level, change node to X

-- #QUESTION: Work with a tree for a while, keep a paper copy of the tree as a note alongside your work

attach :: Tree a -> Zipper a -> Zipper a -- Take a tree and a zipper, return a new zipper where we focus on that particular tree
attach t (_, bs) = (t, bs)               -- This lets us replace empty and existing sub-trees with entirely new trees

-- let farLeft = (freeTree,[]) -: goLeft -: goLeft -: goLeft -: goLeft
-- let newFocus = farLeft -: attach (Node 'Z' Empty Empty)    -> Go to the very left of our tree, attach a node with no sub-branches