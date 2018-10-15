{-# OPTIONS_GHC -fwarn-incomplete-patterns #-} 
module Ex2Test where

import Prelude hiding (words, lines, unlines)
import Ex2

{----------------------------------------------------------------------}
{- CS316 (2018/19) EXERCISE 2 : FIRST-ORDER PROGRAMMING               -}
{-                                                                    -}
{-    * * *  TEST QUESTIONS  * * *                                    -}
{----------------------------------------------------------------------}

-- Submit by committing to GitLab at or before 2pm on Monday 15th
-- October.  There will be a test on this exercise in the lab on that
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
{- PART 1 : FUN(ctional) WITH LISTS                                   -}
{----------------------------------------------------------------------}

{- 2.1.6 TEST: Lines, Unlines.

   Write a pair of functions 'lines' and 'unlines' that respectively
   separate a string into lines by splitting at newline ('\n')
   characters and join a list of lines into a string by inserting
   newlines. -}

lines :: String -> [String]
lines = undefined

unlines :: [String] -> String
unlines = undefined

{- 1 MARK -}

{----------------------------------------------------------------------}
{- PART II : CURSORS                                                  -}
{----------------------------------------------------------------------}

{- 2.2.3 (TEST) Inserting Strings.

   Using your 'insert' function, write a function that inserts a whole
   string before the cursor, as if it had been typed there. -}

inserts :: [a] -> Cursor a -> Cursor a
inserts = undefined

{- 2 MARKS -}

{- 2.2.4 (TEST) Overwriting.

   Write another editing function that /replaces/ the element
   underneath the cursor with the given one. If the cursor is at the
   end of the line, it should at as if the new character replaces the
   'virtual' character at the end of the line. -}

overwrite :: a -> Cursor a -> Cursor a
overwrite = undefined

{- 2 MARKS -}

{- 2.2.6 (TEST) Backspace.

   Write a function that edits the cursor in the same way as your
   backspace key does. That is, it removes the character to the left
   of the cursor. Remember to think carefully about the possible edge
   cases. You may want to experiment with the backspace key in your
   text editor. Be careful not to delete the rest of your answers! -}

backspace :: Cursor a -> Cursor a
backspace = undefined

{- 3 MARKS -}

{- 2.2.7 (TEST) moveEnd

   Using 'moveRight' and 'getPoint', implement a recursive function
   that moves the cursor to the end of the string. -}

moveEnd :: Cursor a -> Cursor a
moveEnd = undefined

{- 2 MARKS -}

{- 2.2.8 (TEST) moveRightUntil

   Using 'getPoint' and 'moveRight', implement a recursive function
   that moves the cursor right until it finds a character that matches
   the given one. -}

moveRightUntil :: Eq a => a -> Cursor a -> Cursor a
moveRightUntil = undefined

{- 3 MARKS -}

{----------------------------------------------------------------------}
{- PART 3 : REPRESENTING PROCESSES                                    -}
{----------------------------------------------------------------------}

{- 2.3.5 (TEST) Expectations. Write a function that given a list of bits,
   generates a process that reads that many bits from the input and
   outputs 'True' if all the bits match the input list, and 'False'
   otherwise. You should have:

      process (expects [True])       [True]       == [True]
      process (expects [True, True]) [True,False] == [False]
      process (expects [True, True]) [True,True]  == [True]
      process (expects [])           [True,False] == [True]

   Remember to always read all the bits!
-}

expects :: [Bool] -> Process
expects = undefined

{- 3 MARKS -}

{- 2.3.7 (TEST) Maximum inputs

   Write a function that performs a 'static analysis' of a process
   (i.e., determines some property of a process without running it on
   actual data. Your function should compute the maximum number of
   bits the process will input on *any* run.

   You may find the function 'max' useful: 'x `max` y' returns the
   maximum of 'x' and 'y'. -}

maxInputs :: Process -> Int
maxInputs = undefined

{- 3 MARKS -}

{----------------------------------------------------------------------}
{- END OF TEST                                                        -}
{----------------------------------------------------------------------}
