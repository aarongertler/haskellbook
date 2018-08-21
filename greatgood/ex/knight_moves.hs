-- Let's represent knight moves in Haskell (and catch up to where we left off in Ruby class practice...)

type KnightPos = (Int,Int)

-- The monad way to do this (in "do" notation):

moveKnight :: KnightPos -> [KnightPos] -- Return a list of places our knight can be after a move
moveKnight (h,v) = do
	(h',v') <- [(h+2,v-1),(h+2,v+1),(h+1,v-2),(h+1,v+2)
	           ,(h-1,v-2),(h-1,v+2),(h-2,v-1),(h-2,v+1)
	           ]
	guard (elem h' [1..8] && elem v' [1..8]) -- Filter moves that would take us off the board
	return (h',v')