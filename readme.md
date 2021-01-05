# Advent of Code 2020

## Days 1 - 7

I started writing notes on the 8th day, so I'm planning to go back and cover
the first week when I have a chance.


## Day 8:

Part 1 was pretty easy, but part 2 required a lot more thought -- and it's
still wildly inefficient. Storing the entire call log along with the
instructions is using 80 megabytes.

This was my first time using the `State` monad in Haskell.


## Day 9:

I ended up solving this one almost entirely inside GHCI, and it shows.

I was tempted to use the [Foldl](https://hackage.haskell.org/package/foldl)
module for the min/max operation after reading about it, but I'm trying to do
these without straying too far from the standard library, and this would
definitely be overkill.

The sliding window function was a clever thing I found online, and was really
the key to solving this challenge. It's going into my notes for future use.

```haskell
import Data.List (tails)

windows :: Int -> [a] -> [[a]]
windows m = foldr (zipWith (:)) (repeat []) . take m . tails

take 4 $ windows 3 [1..] == [[1,2,3],[2,3,4],[3,4,5],[4,5,6]]
```
