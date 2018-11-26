{-# OPTIONS_GHC -fwarn-incomplete-patterns #-} 
module Ex4Test where

import qualified Data.Map as M
import           Data.Map (Map)
import Ex4

{----------------------------------------------------------------------}
{- CS316 (2018/19) EXERCISE 4 : GHOUL                                 -}
{-                                                                    -}
{-    * * *  TEST QUESTIONS  * * *                                    -}
{----------------------------------------------------------------------}

-- Submit by committing to GitLab at or before 2pm on Monday 26th
-- November.  There will be a test on this exercise in the lab on that
-- date.
--
-- Your combined score from the submission and the test will be worth
-- 35% of the overall marks for the class (so one mark, below is worth
-- half a percent).
--
-- This file contains the test questions. Answer the questions in this
-- file, and make sure that both are committed to GitLab both by the
-- end of the lab session.

{- READ THIS FIRST:

       YOU NEED TO REPLACE THE LINE:

           module Main where

       AT THE START OF Ex4.hs WITH:

           module Ex4 where

       BEFORE PROCEEDING. -}

{----------------------------------------------------------------------}
{- Part 2 : PATTERN MATCHING                                          -}
{----------------------------------------------------------------------}

{- 4.2.2 (a) Catch-all patterns

   GHOUL does not allow catch-all patterns that do not bind a variable
   like Haskell's '_'. Adjust the Pat datatype and the 'matchPattern'
   function, and your pattern parser 'pPat', to make the following kind
   of definition work:

       (alwaysOne _) -> (S Z)

   Write a summary of what you changed here so we know. -}

{- 3 MARKS -}



{- 4.2.2 (b) Repeated variables in patterns

   The basic version of GHOUL does not allow definitions like the
   following, where we repeat a variable name in the patterns to
   indicate that both arguments should be equal.

      (isEqual x x) -> True
      (isEqual x y) -> False

   This restriction is enforced by the 'bindVar' function that fails
   if a variable is matched more than once. Write a new version of
   bindVar that allows repeated binding of a variable, as long as the
   values matched are equal. -}

bindVarAllowRepeats :: String -> Value -> Env -> ErrorOr Env
bindVarAllowRepeats x v =
  undefined -- fill this in

{- 2 MARKS -}

{----------------------------------------------------------------------}
{- Part 3: EVALUATION OF EXPRESSIONS                                  -}
{----------------------------------------------------------------------}

{----------------------------------------------------------------------}
{- 4.3.1 (a) Write a GHOUL program that reverses a list. See the
   function 'rev' defined in Lecture 02. You will need to also include
   the 'append' function from Question 4.0.0. You can either write the
   function in GHOUL syntax (as a string), or as a value of type
   'Program'. -}

revProg :: Program -- replace with 'String', if you want
revProg = undefined

{- Also write a test case for this program and the expected output here:


-}

{- 4 MARKS -}

{- 4.3.1 (b) "Panic" function. Add a case to the 'eval' function that
   evaluates any function called 'panic' specially: it evaluates all
   the arguments, and then aborts (using 'abortEval') with an error
   message containing all the values that the arguments evaluated to
   pretty printed using 'ppValue'.

   Example:

      > runEval (eval M.empty (EA "panic" [EC "S" [EC "Z" []]])) []
      Error "PANIC: (S Z)"

      > runEval (eval M.empty (EA "panic" [EA "panic" [EC "AARRGH" []]])) []
      Error "PANIC: AARRGH"

   Write the changes you have made here: -}

{- 3 MARKS -}
{----------------------------------------------------------------------}

{----------------------------------------------------------------------}
{- Part 4 : PARSING                                                   -}
{----------------------------------------------------------------------}

{----------------------------------------------------------------------}
{- 4.4.6 Desugaring lists

   GHOUL allows the user to create lists by explicitly using 'Cons'
   and 'Nil' constructors. For example, the program

     (main) -> (Cons A (Cons B (Cons C Nil)))

   returns the list [A B C].

   However, GHOUL does not allow the user to use the nice syntax [A B
   C]. (Note that we're not using commas here, just like in the rest
   of GHOUL.)

   For this question, modify the parser you have written to allow list
   literals like '[]', '[A B C D]', '[(S Z) (S (S Z))]', '[(S Z) x]'
   to appear in patterns and expressions. The parser should convert
   list literals to the appropriate use of 'Cons' and 'Nil'. For
   example:

      > runParser pPat "[]"
      OK (PC "Nil" [],"")

      > runParser pPat "[A]"
      OK (PC "Cons" [PC "A" [],PC "Nil" []],"")

      > runParser pPat "[A B]"
      OK (PC "Cons" [PC "A" [],PC "Cons" [PC "B" [],PC "Nil" []]],"")

      > runParser pPat "[x]"
      OK (PC "Cons" [PV "x",PC "Nil" []],"")

      > runParser pPat "[x y]"
      OK (PC "Cons" [PV "x",PC "Cons" [PV "y",PC "Nil" []]],"")

      > runParser pPat "[(S Z) x (S (S Z))]"
      OK (PC "Cons" [PC "S" [PC "Z" []],PC "Cons" [PV "x",PC "Cons" [PC "S" [PC "S" [PC "Z" []]],PC "Nil" []]]],"")

   and similar for expressions.

   NOTE: you do not need to alter the 'Pat' or 'Exp' datatypes, only
   the 'pPat' and 'pExp' functions, plus some auxillary functions
   (described below).

   HINT: First define an auxillary function called 'mkPatList' of type
   '[Pat] -> Pat' that takes a list of patterns and produces a single
   pattern using 'PC "Cons" [_, _]' and 'PC "Nil" []' (you fill in the
   '_'s). Then extend 'pPat' to recognise lists of patterns surrounded
   by square brackets and separated by spaces, and use 'mkPatList' to
   turn the list of patterns into a 'Pat'tern. Then do the same for
   'Exp'ressions.

   Write the changes you made here: -}


{- 6 MARKS -}
{----------------------------------------------------------------------}

{----------------------------------------------------------------------}
{- 4.5.2 functionCheck 

   Write a function that checks that all the function names used on the
   right-hand side of every rule have at least one equation defined
   for them in the program.

   For example:

    > functionCheck []
    OK ()

    > functionCheck [MkRule "main" [] (EA "notDefined" [])]
    Error "Function notDefined undefined"

    > functionCheck plusProgramAST
    OK ()
-}

functionCheck :: Program -> ErrorOr ()
functionCheck = undefined

{- 4 MARKS -}
{----------------------------------------------------------------------}
