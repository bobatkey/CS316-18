module Main where

import System.Environment
import Control.Parallel.Strategies
import Debug.Trace

{-    LECTURE 19 : Parallelism

   This lecture is about running Haskell programs in parallel on
   multiple cores.

   Parallel programming is used to make programs run faster by
   splitting up their work so that it can be executed on multiple
   computing devices. It is not to be confused with concurrency --
   which is programming with overlapping processes that communicate
   with each other.

   The main source for this lecture is the book:

     "Parallel and Concurrent Programming in Haskell"
     by Simon Marlow
        http://chimera.labs.oreilly.com/books/1230000000929

   which is available to read online for free. -}

{--------------------------------------------------------------------}
{- Part 1. Controlling Evaluation Order

   Recall from Lecture 17 that Haskell uses "Lazy Evaluation". This
   means that (a) nothing is computed until it is needed; and (b)
   nothing is computed more than once. Sometimes laziness means that
   unevaluated computations can build up, wasting space. We saw the
   strict application function ($!) and the odd function 'seq' for
   controlling evaluation order. -}

traceWhenDone :: (Show a, Show b) => String -> (a -> b) -> (a -> b)
traceWhenDone name f a =
  trace (name ++ " " ++ show a ++ " = " ++ show result) result
  where result = f a

tracedFib1, tracedFib2 :: Int -> Int
tracedFib1 = traceWhenDone "fib1" fib
tracedFib2 = traceWhenDone "fib2" fib

{- The most basic mechanism for controlling evaluation order in
   Haskell exploits the graph structure created for laziness to mark
   parts of the computation that can be executed in parallel.

   The Eval monad is a monad for providing parallelism hints and
   control evaluation order. (It is built on two lower-level
   primitives 'par' and 'pseq', which we won't cover in this lecture.)

   The 'Eval' monad provides these basic operations:

     runEval :: Eval a -> a

     rpar :: a -> Eval a
 
     rseq :: a -> Eval a

   'rpar' means "you could evaluate my argument in parallel"

   'rseq' means "evaluate my argument and wait for it to get to WHNF"

   We can use these to describe different evaluation mechanisms: -}

sched0,sched1,sched2,sched3,sched4,sched5
  :: (a -> b) -> (a -> c) -> a -> (b,c)

sched0 f g x = (f x, g x)

{- This doesn't run anything until the values in the pair are
   requested. They are evaluated on demand while the requester waits
   for the answers. -}


sched1 f g x =
  runEval $ do a <- rseq (f x)
               b <- rseq (g x)
               return (a, b)

{- This runs 'f x' until it gets to a constructor, then it runs 'g x'
   until it gets to a constructor, then it returns the pair of WHNF
   values generated. -}


sched2 f g x =
  runEval $ do a <- rpar (f x)
               b <- rpar (g x)
               return (a, b)

{- This forks off a thread to run 'f x' in parallel, forks off a thread
   to run 'g x' in parallel, and then returns pointers to the boxes
   waiting for these two jobs. -}


sched3 f g x =
  runEval $ do a <- rpar (f x)
               b <- rseq (g x)
               return (a, b)

{- This forks off a thread to run 'f x', runs 'g x' to get a
   constructor. -}


sched4 f g x =
  runEval $ do a <- rpar (f x)
               b <- rseq (g x)
               rseq a
               return (a, b)

{- This forks off a thread to run 'f x', runs 'g x' to completion, and
   then waits for 'a' (which is a pointer to 'f x') to finish. -}


sched5 f g x =
  runEval $ do a <- rpar (f x)
               b <- rpar (g x)
               rseq a
               rseq b
               return (a, b)

{- This forks threads to run 'f x' and 'g x', then waits for both to
   finish and returns the results. -}

{--------------------------------------------------------------------}
{- Part 2 : FIBONACCI !

   Let's try to parallelise fibonacci. -}

fib :: Int -> Int
fib 0 = 1
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

pfib :: Int -> Int -> Int
pfib d 0 = 1
pfib d 1 = 1
pfib 0 n = pfib 0 (n-1) + pfib 0 (n-2)
pfib d n = runEval (do
  x1 <- rpar (pfib (d-1) (n-1))
  x2 <- rseq (pfib (d-1) (n-2))
  return (x1 + x2))

main = do
  n <- read . head <$> getArgs
  print (pfib 2 n)
