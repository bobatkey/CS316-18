{-# OPTIONS_GHC -fwarn-incomplete-patterns #-} 
module Ex3Test where

import Prelude hiding (words, lines, unlines)
import Ex3

{----------------------------------------------------------------------}
{- CS316 (2018/19) EXERCISE 3 : HIGHER-ORDER PROGRAMMING              -}
{-                                                                    -}
{-    * * *  TEST QUESTIONS  * * *                                    -}
{----------------------------------------------------------------------}

-- Submit by committing to GitLab at or before 2pm on Monday 5th
-- November.  There will be a test on this exercise in the lab on that
-- date.
--
-- Your combined score from the submission and the test will be worth
-- 30% of the overall marks for the class (so one mark, below is worth
-- half a percent).
--
-- This file contains the test questions. Answer the questions in this
-- file, and make sure that both are committed to GitLab both by the
-- end of the lab session.

{----------------------------------------------------------------------}
{- 1. STRUCTURAL RECURSION ON TREES AND LISTS                         -}
{----------------------------------------------------------------------}

{- 3.1.4 Trees full of functions. -}

{- Define 'applyTree', a function that takes a binary tree of binary
   functions, and a value to use for the leaves, and returns the value
   computed by recursively applying the function at each node to the
   values computed for its two sub-trees.

   For example:

     applyTree (Node (Node Leaf (+) Leaf) (*) (Node Leaf (+) Leaf)) 1 = 4

   because: (1 + 1) * (1 + 1) = 2 * 2 = 4.

   Define your 'applyTree' using 'iterTree'. -}

applyTree :: Tree (Int -> Int -> Int) -> Int -> Int
applyTree = undefined

{- 2 MARKS -}

{----------------------------------------------------------------------}
{- 2. COMPARATORS AND SORTING                                         -}
{----------------------------------------------------------------------}

{- 3.2.4 Using 'qsortWith' and the other functions above, write a
   function that sorts lists of '(Int,String)' on the length of the
   second element of each pair. -}

sortOnSndLength :: [(Int,String)] -> [(Int,String)]
sortOnSndLength = undefined

{- Example:

       > sortOnSndLength [(1,"one"), (2,"two"), (3,"three"), (4,"four"), (5, "five"), (6, "six")]
       [(1,"one"),(2,"two"),(6,"six"),(4,"four"),(5,"five"),(3,"three")]
-}

{- 2 MARKS -}

{----------------------------------------------------------------------}
{- PART 3 : A PICTURE LANGUAGE                                        -}
{----------------------------------------------------------------------}

{- 3.3.7 Boolean Combinations of Bitmaps.

   Implement the following functions that compute the 'AND', 'OR' and
   'NOT' of two bitmaps of 'Bool's.

   Use your functions to create a bitmap consisting of a square of the
   given size with a circular hole in it. The hole should be smaller
   that the size of the square. -}

andBMP :: Bitmap Bool -> Bitmap Bool -> Bitmap Bool
andBMP = undefined

orBMP :: Bitmap Bool -> Bitmap Bool -> BitMap Bool
orBMP = undefined

notBMP :: Bitmap Bool -> Bitmap Bool
notBMP = undefined

squareWithARoundHole :: Double -> Bitmap Bool
squareWithARoundHole r = undefined

{- 5 MARKS -}

{- 3.3.11 Flipping. Write functions that flip a 'Bitmap'. The first
    function should flip top to bottom (and bottom to top). The second
    should flip left to right (and right to left).

    Use 'transform' to write your functions. -}

flipTopBottom :: Bitmap a -> Bitmap a
flipTopBottom = undefined

flipLeftRight :: Bitmap a -> Bitmap a
flipLeftRight = undefined

{- 2 MARKS -}

{----------------------------------------------------------------------}
{- 4. PROCESSES                                                       -}
{----------------------------------------------------------------------}

{- 3.4.2 Process translation. Here is the 'Process' type from Exercise
   2, renamed to prevent a clash with our new 'Process' type. -}

data BProcess
  = BEnd
  | BOutput Bool BProcess
  | BInput BProcess BProcess
  deriving Show

{- Write a function that translates a 'BProcess' into a 'Process Bool
   ()'. Whenever the Process ends, the 'CP' process should end;
   whenever the 'Process' outputs, the 'CP' process should output; and
   whenever the 'Process' inputs, the 'CP' process should input.

   In the other direction, write a function that translates a 'Process
   Bool ()' to a 'BProcess'. -}

bprocessToProcess :: BProcess -> Process Bool ()
bprocessToProcess = undefined

processToBProcess :: Process Bool () -> BProcess
processToBProcess = undefined

{- 4 MARKS -}


{- 3.4.4 Define a process that does the same thing as 'echo' above but
   only using 'input', 'output' and 'sequ'. -}

echoFromSequ :: Process x ()
echoFromSequ = undefined

{- 1 MARK -}


{- 3.4.5 Sequencing without values. Some processes, like 'output x'
   don't return any interesting value (a bit like a function that
   returns 'void' in Java or C). In this case, using 'sequ' to
   sequence processes is a bit clumsy because we have to write an
   anonymous function that ignores its argument:

      p1 `sequ` \_ -> p2

   Write a function 'sequ_' that uses 'sequ' to sequence two processes
   where the output of the first is ignored. -}

sequ_ :: Process x () -> Process x a -> Process x a
sequ_ p1 p2 = undefined

{- 1 MARK -}



{- 3.4.9 Translation of processes. Sometimes, we might have a process
   that sends and receives values of type 'x', but we want a process
   that sends and receives values of type 'y'. Define a function
   'translate' that takes two translation functions, one from 'x's to
   'y's and one from 'y's to 'Maybe x's, and converts processes that
   communicate using 'x's into processes that communicate using
   'y's. If the translation fails (because the second function returns
   'Nothing'), then the process should abort by 'End'ing with Nothing. -}

translate :: (x -> y) -> (y -> Maybe x) -> Process x a -> Process y (Maybe a)
translate xToy yTox p = undefined

{- 3 MARKS -}

{- 3.4.10 Below we have defined functions stringToMaybeInt and
   intToString that translate back and forth between strings and
   integers. Use these functions and translate to define
   'intsToStrings'. -}

stringToInt :: String -> Maybe Int
stringToInt = readMaybe

intToString :: Int -> String
intToString = show

intsToStrings :: Process Int a -> Process String (Maybe a)
intsToStrings = undefined

{- 1 MARK -}

{----------------------------------------------------------------------}
{- END OF TEST                                                        -}
{----------------------------------------------------------------------}
