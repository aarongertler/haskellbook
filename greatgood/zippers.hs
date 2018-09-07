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

elemAt :: Directionos -> Tree a -> a
elemAt (L:ds) (Node _ l _) = elemAt ds l -- If current first direction in the list is "left"...
elemAt (R:ds) (Node _ _ r) = elemAt ds r
elemAt [] (Node x _ _) = x  -- Stop and give your value once you reach the end of the directions