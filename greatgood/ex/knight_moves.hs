-- Let's represent knight moves in Haskell (and catch up to where we left off in Ruby class practice...)

import Control.Monad

type KnightPos = (Int,Int)

-- The monad way to do this (in "do" notation):

moveKnight :: KnightPos -> [KnightPos] -- Return a list of places our knight can be after a move
moveKnight (h,v) = do
	(h',v') <- [(h+2,v-1),(h+2,v+1),(h+1,v-2),(h+1,v+2)
	           ,(h-1,v-2),(h-1,v+2),(h-2,v-1),(h-2,v+1)
	           ]
	guard (elem h' [1..8] && elem v' [1..8]) -- Filter moves that would take us 

in3 :: KnightPos -> [KnightPos] -- This should be easy to swap to inX, right? Still don't have a good grasp of Haskell loops 
in3 start = return start >>= moveKnight >>= moveKnight >>= moveKnight 

-- TRY TO MAKE THE ABOVE WORK BEFORE YOU MOVE ON


-- in3 start = do
-- 	first <- moveKnight start
-- 	second <- moveKnight first
-- 	moveKnight second -- will return a list of all places we can reach in three moves
