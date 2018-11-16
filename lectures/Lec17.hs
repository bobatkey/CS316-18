module Lec17 where

import Prelude hiding (const, take, iterate)
import Debug.Trace

{-     LECTURE 17 : LAZY EVALUATION AND INFINITE DATA

   This lecture is about how Haskell evaluates programs, which is
   not the same as how most programming languages work. Haskell employs
   'lazy evaluation', which means that values are never computed
   unless they are needed, and the 'same' value is never computed more
   than once.

   CREDITS: The 'inc', 'neverFinish', 'square' and 'sumList' examples
   are taken from Hutton's "Programming in Haskell", 2nd ed, Chapter
   15. The 'findSqrt' example is taken from the paper "Why Functional
   Programming Matters" by John Hughes (link in the online notes). -}

{- A simple function: -}

inc :: Int -> Int
inc n = n + 1

{- How is the following evaluated?

     inc (2*3)
-}

{- 'Call by Value'

       inc (2*3)
     =              { multiply 2 and 3 }
       inc 6
     =              { definition of 'inc' }
       6 + 1
     =              { add }
       7

   'Call-by-Value' is so-called because it evaluates the arguments of
   functions to values before applying them. -}

{- 'Call by Name'

       inc (2*3)
     =              { definition of 'inc' }
       (2*3) + 1
     =              { multiply }
       6 + 1
     =              { add }
       7

   'Call-by-Name' is so called because it just passes expressions
   whole to functions (i.e., a 'name' for the value). -}

{- We could, in theory, mix Call-by-Value and Call-by-Name, but most
   languages pick one or the other to be the default, and offer the
   other via some special mechanism. -}

{--------------------------------------------------------------------}
{- Termination behaviour.

   Is there any difference between the two strategies? For programs
   that terminate under both, no, but there is a difference when we
   have programs that may not terminate. -}

neverFinish :: Int
neverFinish = 1 + neverFinish

const :: a -> b -> a
const a b = a

{- What does this do?

      const 1 neverFinish
-}

{- 'Call by Value'

       const 1 neverFinish
     =
       const 1 (1 + neverFinish)
     =
       const 1 (1 + (1 + neverFinish))
     =
       ...
     =
       const 1 (1 + ... (1 + neverFinish))
     =
       ...
-}

{- 'Call by Name'

       const 1 neverFinish
     =                        { definition of 'const' }
       1
-}

{- If there is *any* evaluation sequence that terminates, then CBN will
   also terminate and give the same answer. -}

{--------------------------------------------------------------------}
{- Sharing

   Naive Call-by-Name would lead to repeated work: -}

square :: Int -> Int
square x = x * x

{-
    square (2*3)
  =                  { definition of square }
    (2*3) * (2*3)
  =                  { multiply }
    6 * (2*3)
  =                  { multiply }
    6 * 6
  =                  { multiply }
    36
-}

{- But with Call-by-Value:

     square (2*3)
  =                  { multiply }
     square 6
  =                  { definition of square }
     6 * 6
  =                  { multiply }
     36
-}

{- So Haskell uses sharing to avoid repeatedly evaluating the same
   expression:

     square (2*3)
   =                          { give '2*3' a name so it can be shared }
     let x = 2*3 in square x
   =                          { definition of square }
     let x = 2*3 in x * x
   =                          { multiply (forced by '*') }
     let x = 6 in x * x
   =                          { fetch 'x' }
     let x = 6 in 6 * 6
   =                          { multiply }
     let x = 6 in 36
   =                          { garbage collect }
     36


   This evaluation strategy is called 'Lazy evaluation':

      - Expressions are not evaluated until needed.
      - Expressions are not evaluated more than once.

   This strategy is realised by sharing computations that may be used
   more than once. This can be seen by using a feature of GHCi that
   allows us to print the value of a variable without evaluating it,
   and the ability to add a side-effecting trace message to any
   value that gets printed when it is evaluated.

   First we enter two expressions and give them names:

       λ> let x = 5 :: Int
       λ> let y = x * x

   We had to explicitly say that '5' is an 'Int' because otherwise
   Haskell doesn't know if we mean '5 :: Int', '5 :: Double', '5 ::
   Float', or '5 :: Integer'.

   Now if we look at 'x' with :sprint we can see that it is a value:

       λ> :sprint x
       x = 5

   But if we look at 'y', we can see that it has not yet been
   evaluated:

       λ> :sprint y
       y = _

   If we ask for 'y' explicitly:

       λ> y
       25

   Then using :sprint again will show us that 'y' now points to a
   value:

       λ> :sprint y
       y = 25

   This demonstrates that values are not evaluated unless they are
   needed. Another example is the following, which creates two
   suspended computations 'y' and 'z', and then only uses one of them:

       λ> let x = 5 :: Int
       λ> let y = x * x
       λ> let z = x + x
       λ> let a = const y z

   Inspecting 'y', 'z', and 'a' shows that no computation has happened
   yet:

       λ> :sprint y
       y = _
       λ> :sprint z
       z = _
       λ> :sprint a
       a = _

   If we request the value of 'a', then we get it:

       λ> a
       25

   And we can see that 'y' and 'a' are now resolved to values:

       λ> :sprint y
       y = 25
       λ> :sprint a
       a = 25

   But 'z' is still unevaluated:

       λ> :sprint z
       z = _

   These examples show us that values are not evaluated unless they
   are needed, but not that they are only evaluated once. To see this,
   we can use a special function defined in the 'Debug.Trace' module
   that allows us to attach a string to be printed whenever an
   expression is evaluated.

   The 'trace' function takes a string and a value and returns the
   value. As a side effect it also prints the string. Note that this
   doesn't use the IO monad to do side-effects -- it is strictly
   speaking not a 'pure' functional programming. However it is very
   useful for debugging.

   Here's how to use it. We create three named expressions, 'x', 'y',
   'z', but we wrap 'y' in a trace function. When we request the value
   of 'z', this requests the value of 'y' twice. Due to laziness, we
   only do the work once. We can see this because "Evaluating 'y'" is
   only printed once. The second time, 'y' just returns '25', and
   nothing is printed.

       λ> let x = 5 :: Int
       λ> let y = trace "Evaluating 'y'" (x * x)
       λ> let z = y + y
       λ> z
       Evaluating 'y'
       50
-}

{--------------------------------------------------------------------}
{- Laziness, Procrastination, and Strictness -}

sumList :: Int -> [Int] -> Int
sumList accum []     = accum
sumList accum (x:xs) = sumList (accum + x) xs

{- Evaluation under Call-by-Value:

     sumList 0 [1,2,3]
   =
     sumList (0+1) [2,3]
   =
     sumList 1 [2,3]
   =
     sumList (1+2) [3]
   =
     sumList 3 [3]
   =
     sumList (3+3) []
   =
     sumList 6 []
   =
     6
-}

{- Evaluation under Call-by-Name or Lazy Evaluation:

     sumList 0 [1,2,3]
   =
     sumList (0+1) [2,3]
   =
     sumList ((0+1)+2) [3]
   =
     sumList (((0+1)+2)+3) []
   =
     ((0+1)+2)+3
   =
     (1+2)+3
   =
     3+3
   =
     6
-}

{- For long lists, the 0+1+2+3+4+5+... builds up and is not evaluated
   until the end of the list. This can consume a large amount memory,
   and is known as a "space leak".

   This can lead to surprising behaviour, and can cause seemingly
   simple programs to run out of memory and crash.

   The fix in this case is to use strict application: -}

sumStrict :: Int -> [Int] -> Int
sumStrict accum []     = accum
sumStrict accum (x:xs) = (sumStrict $! (accum+x)) xs

{- The strict application operator:

      ($!) :: (a -> b) -> a -> b

   is 'magic' in the sense that it cannot be implemented in 'normal'
   Haskell. It evaluates the second argument before applying the
   function to it. With strict evaluation we get the 'Call-by-Value'
   behaviour as above.

   This function is actually implemented in terms of Haskell's basic
   function for forcing evaluation, called 'seq':

       seq :: a -> b -> b

   'seq' always just returns its second argument, but only if its
   first argument can be evaluated to a 'head' value (this means that
   it only goes as far as the top most constructor. So, for example,
   if the first argument does not terminate, then 'seq' does not
   terminate:

       λ> seq neverFinish 1
       <Ctrl-C>
       Interrupted.

   However, 'seq' is shallow in the sense that it only looks to
   evaluate its first argument to the 'first constructor'. For
   example:

       λ> seq [1..] 1
       1

   Even though '[1..]' is an infinite list and can never be completely
   evaluated, it can be evaluated until it gets to the first ':'
   constructor. At this point, 'seq' returns its second argument.

   We can now use 'seq' to implement '$!':

      ($!) f a = a `seq` f a

   So 'seq' forces the argument 'a', and then applies 'f' to 'a'. Due
   to sharing, this means that the 'a' that 'f' sees has been
   evaluated down to a 'head' value.

   (Note: in versions of GHC >= 7.10, this is not actually how '$!' is
   implemented, due to interactions with GHC's optimiser. The '$!' in
   the standard library is actually implemented using strict pattern
   matching:

       ($!) f a = let !va = a in f va

   See https://ghc.haskell.org/trac/ghc/ticket/2273 for more details)
-}

{--------------------------------------------------------------------}
{- Infinite Data

   A benefit of lazy evaluation is the ease of handling infinite data,
   and modularity benefits this can give to programs.

   Here is a function that generates infinite lists: -}

upFrom :: Int -> [Int]
upFrom i = i : upFrom (i+1)

{- Trying to print out 'upFrom 0' will never terminate, but we can use
   various other functions to slice off bits of it. For example,
   'take' takes some prefix of the list: -}

take :: Int -> [a] -> [a]
take 0 _      = []
take n (x:xs) = x : take (n-1) xs

{- So:

       λ> take 5 (upFrom 0)
       [0,1,2,3,4]

   The benefit of laziness is that we don't have to make the decision
   that we are going to only take 5 elements of the list until the
   very end. If we were to implement this in a language that could
   only handle finite data, we would have to change 'upFrom' to take
   the number of elements that we needed. With laziness, we can do
   multiple manipulations to the list before deciding how many
   elements to use:

       λ> let numbers = upFrom 0
       λ> let evens = filter (\x -> x `mod` 2 == 0) numbers
       λ> let odds = filter (\x -> x `mod` 2 == 1) numbers
       λ> let square_evens = map (\x -> x * x) evens
       λ> let added_up = map (\(x,y) -> x+y) (zip square_evens odds)
       λ> take 10 added_up
       [1,7,21,43,73,111,157,211,273,343]

   Laziness can be useful for making programs more modular. Here
   is an example of finding square roots by generating an infinite
   list of approximations and then, separately, deciding how to cut it
   off (taken from "Why Functional Programming Matters" by John
   Hughes:

      https://www.cs.kent.ac.uk/people/staff/dat/miranda/whyfp90.pdf

   which is quite readable and uses a syntax very similar to Haskell.)

   The following function takes a number 'n' and a guess 'x' at the
   square root of 'n' and returns a better guess: -}

next :: Double -> Double -> Double
next n x = (x + n/x)/2

{- If we can find a non-zero value 'x' such that

       next n x = x

   Then we have the square root of 'n'. This is because:

        (x + n/x)/2 = x
    <=>
        x + n/x = 2*x
    <=>
        n/x = x
    <=>
        n = x*x

   From the theory of Newton-Raphson root finding algorithms, we can
   approximate this value by starting from some initial guess and then
   repeatedly applying 'next n'.

   We can use this idea to generate an infinite list of
   approximations: -}

-- This is defined in the standard library:
iterate :: (a -> a) -> a -> [a]
iterate f a = a : iterate f (f a)

{- So 'iterate (next 2) 1' will give us an infinite list of
   approximations of the square root of 2, starting with the initial
   guess '1':

       λ> iterate (next 2) 1
       [1.0,1.5,1.4166666666666665,1.4142156862745097,1.4142135623746899,
        1.414213562373095,1.414213562373095,1.414213562373095,
        1.414213562373095,1.414213562373095
       <Ctrl-C>
       Interrupted

   But how do we know when to stop?

   One way is to stop when the difference between two approximations
   is smaller than 'some small number': -}

within :: Double -> [Double] -> Double
within eps (a:b:xs) | abs (a-b) < eps = b
within eps (_:b:xs)                   = within eps (b:xs)

{- Now we can plug together 'within' and 'iterate (next n) 1' to make a
   square root finder: -}

findSqrt :: Double -> Double
findSqrt n = within 0.0000001 (iterate (next n) 1)

{- And it works if we check against the built-in 'sqrt' function:

      λ> findSqrt 2
      1.414213562373095
      λ> sqrt 2
      1.4142135623730951

   However, when the number is small, using 'within' to cut off the
   search doesn't necessarily give a good answer:

      λ> findSqrt 0.00001
      3.1622776602038957e-3
      λ> sqrt 0.00001
      3.1622776601683794e-3

   When the numbers are small, a better strategy is to cut off when
   the ratio between two numbers in the sequence is close to 1: -}

relative :: Double -> [Double] -> Double
relative eps (a:b:xs) | abs (a/b - 1) < eps = b
relative eps (_:b:xs)                       = relative eps (b:xs)

{- We can now build another square root finder that works better for
   small numbers. Note that we did not have to change how we generated
   the sequence of approximations, only the check at the end. Laziness
   has allowed us to separate generating the approximations from
   checking them: -}

findSqrt2 :: Double -> Double
findSqrt2 n = relative 0.0000001 (iterate (next n) 1)

{- We can now see that 'findSqrt2' does a better job on small numbers:

       λ> findSqrt2 0.00001
       3.1622776601683794e-3
       λ> sqrt 0.00001
       3.1622776601683794e-3
-}
