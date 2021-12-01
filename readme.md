# Advent of Code 2021

## Day 1:

The `windows` function from last year (day 9) ended up being useful already!


# Advent of Code 2020

## Days 1 - 7:

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

## Day 10:

I first read over the puzzle text at some time after 1 AM and couldn't make any
sense of it.  Fortunately after sleeping, it became much clearer.

For the first part, counting the differences between sorted list items was the
obvious solution, and an `IntMap` made sense to store / look up the counts.

The `runLength` function was the most interesting one this time, but especially
the `liftA2 (,)` portion:

```haskell
runLength :: (Eq a) => [a] -> [(a, Int)]
runLength = map (liftA2 (,) head length) . group

liftA2 (,) :: Applicative f => f a -> f b -> f (a, b)
```

That's a nice way to build a tuple from a single input.

Naturally, part 2 was trickier. Brute-forcing using `Data.List.subsequences` and
a `filter` seems possible, but that's 2^108 results to check, and I have better
ways of heating my house.

While taking a walk, I realized the list of differences from part 1 would be
enough to figure out how many possible combinations there could be. All of the
puzzle inputs contained differences of either 1 or 3 between each number, and
the 3's wouldn't matter. I just needed to know what effect of each run of 1's
would have on the total count.

I wrote out all the combinations from a simple example, counting the number of
combinations for each set:

```
1   3
1 2 3

1     4
1   3 4
1 2   4
1 2 3 4

1     4 5
1   3   5
1   3 4 5
1 2     5
1 2   4 5
1 2 3   5
1 2 3 4 5

...
```
...and a pattern emerged, `[1, 2, 4, 7, 13, 24, 44, ...]`. It's like the
Fibonacci sequence, but referencing the previous three numbers instead of the usual
two. Also known as the "tribonacci" sequence, apparently.

So, I mapped the runLengths of ones through my `fib3` function, and multiplied
them all together. Boom. The example answer matched my result, and running it
against the full input was successful as well, and instantaneous.


## Day 11:

Sort of a game of life simulation but with seating charts. As with all of these
I'm sure my solution is naive and slow, and I'm working on a rewrite before I
complete the second part...
https://stackoverflow.com/questions/7442892/repeatedly-applying-a-function-until-the-result-is-stable

## Day 12:

Completed this one after a trip to the beach. The most difficult part of it was
parsing the input correctly; it had been a while since I last worked with
Parsec. I had also initially used `foldr` instead of `foldl'`, which was
applying ship instructions in reverse -- this actually worked for part one, but
since the instructions in part two are order dependant, it was completely wrong
even on the first instruction. I finally realized my mistake after stepping
through the solver function with `Debug.Trace`.


# Advent of Code 2019

## Day 1:

Did this as a warm-up right before the 2021 AoC opened, initially in the REPL.
I haven't written any Haskell in 9 months besides messing with [xmonad](https://github.com/xmonad/xmonad).
