All right! Decided to start with Learn You a Haskell and see how that goes

I'll start with some notes on Haskell before jumping into the code.

What is functional programming?


It's a collection of functions. 
All it ever does is transform data. 
No resetting variables.
Any function you call with the same parameters will always return the same results. 
You can't ever forget that you've set a variable to something else midway.
(That's called referential transparency.)


Haskell is lazy.
It will only calculate things when it absolutely must.
For example, double(double(double(1, 2, 3))) will only pass through the list once.
That's because we only evaluate what happens to each number once (each number is doubled three times).
In some imperative languages, you'd have to pass through the list three times.
Edward Kmett has other thoughts on why laziness is useful: https://stackoverflow.com/posts/265548/revisions
(There are lots of times in programming where we might want to be able to use the result of a function sometimes, without actually calculating it
	when we don't need it. Haskell makes this not only easy, but mandatory.)

This is the most helpful link on laziness I've found: https://www.schoolofhaskell.com/school/starting-with-haskell/introduction-to-haskell/6-laziness


Haskell is concise, or so the author claims.
I guess we'll find out!

(Pause to download the Haskell platform.)