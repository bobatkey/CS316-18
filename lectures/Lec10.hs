module Lec10 where

{-    LECTURE 10 : BUILDING PURE EVALUATORS

   In this lecture, we will look at how to build evaluators
   (a.k.a. interpreters) for a sequence of languages with various
   kinds of 'impure' features. The point of this isn't to (just) see
   how to implement simple programming languages in Haskell, but to
   get an insight into how to program with "non functional" features
   in a language like Haskell. -}

{-    PART I : EVALUATION -}

data Expr
  = Number Int
  | Add    Expr Expr
  deriving Show

{- 'Expr's are binary trees, with 'Int's at the leaves and every node
   labeled with 'Add'. Here is an example 'Expr': -}

myExpr :: Expr
myExpr = Number 23 `Add` (Number 34 `Add` Number 56)

{- representing the expression:

      23 + (34 + 56)

   Note that the bracketing is important. Even though we know that it
   does not matter what order we bracket actual addition, we are not
   doing actual addition yet. 'Expr' is a type of _abstract_ syntax
   trees for expressions. To interpret an 'Expr' using actual
   arithmetic, we have to describe what to do for each constructor in
   'Expr'.

   The 'evaluate' function defined here uses pattern matching to
   interpret each constructor in an 'Expr' with its "meaning". We make
   the decision that the meaning of 'Number n' is just 'n'. The
   meaning of 'Add' is the actual '+' function -- matching our
   intuition about how to interpret addition. -}

evaluate :: Expr -> Int
evaluate (Number n)  = n
evaluate (Add e1 e2) = evaluate e1 + evaluate e2

{- Let's try it out:

      λ> evaluate myExpr
      113

   We've written a small programming langauge! -}


{-    PART II : EVALUATION WITH EXCEPTIONS

   The 'Expr' type above described "pure" arithmetic expressions that
   always evaluate to a value. Often programming languages have
   facilities that allow for "non-pure" side effects to happen during
   the program.

   The following data type 'Expr2' extends 'Expr' with two new
   constructors: 'Throw2' and 'Catch2'. The intention is that 'Throw2'
   represents the action of throwing an exception, and 'Catch2'
   represents a try-catch style exception handler. The first argument
   to 'Catch2' is the expression to try, and the second argument is
   the exception handler. -}

data Expr2
  = Number2 Int
  | Add2    Expr2 Expr2
  | Throw2
  | Catch2  Expr2 Expr2
  deriving Show

{- An example program using Throw2 and Catch2 is this one: -}

myProblemProgram :: Expr2
myProblemProgram =
  (Number2 23 `Add2` (Number2 34 `Add2` Throw2)) `Catch2` (Number2 0)

{- This program attempts to perform the same computation as before, but
   one of the numbers is faulty, so it throws an exception which is
   caught by an exception handler which handles it with a handler that
   always returns '0'.

   To define an evaluator for 'Expr2's, we might start by trying to
   write a function of type:

     evaluate2 :: Expr2 -> Int

   After all, evaluation of Expr2 should still result in integers
   being returned. However, this type does not model the fact that
   evaluation of an 'Expr2' may fail with an exception. We need to
   adjust the type of the return value of 'evalExpr2' to account for
   the possibility of throwing an exception. We do this by stating
   that evaluation returns 'Maybe Int' -- so it can either be
   'Nothing' (when an exception is thrown), or 'Just n' (when
   evaluation returns normally). -}

evaluate2 :: Expr2 -> Maybe Int
{- For the 'Number2' case, we always return 'Just n', because there is
   no way to throw an exception while evaluating a number. -}
evaluate2 (Number2 n)    = Just n
{- For the 'Add2' case, we have to evaluate 'e1' and 'e2', but we also
   have to deal with the possibility that evaluating either of them
   may cause an exception to be thrown, which we should propagate to
   the final answer. We do this by using a cascade of 'case's: -}
evaluate2 (Add2 e1 e2)   = case evaluate2 e1 of
                             Nothing -> Nothing
                             Just n1 -> case evaluate2 e2 of
                                          Nothing -> Nothing
                                          Just n2 -> Just (n1+n2)
{- To interpret 'Throw2', we use 'Nothing' to represent the case when an
   exception is thrown. -}
evaluate2 Throw2         = Nothing
{- Finally, for the 'Catch2' case, we evaluate the first expression. If
   it returns a value, we just return that value. If it fails with
   'Nothing', then we evaluate the exception handler and use its
   result as the result of evaluating the whole 'Catch2' expression. -}
evaluate2 (Catch2 e1 e2) = case evaluate2 e1 of
                             Nothing -> evaluate2 e2
                             Just n  -> Just n

{- Now evaluating our test program shows the exception throwing and
   handling working:

     λ> evaluate2 myProblemProgram
     Just 0

   If we try to evaluate a program that throws an exception with no
   exception handler, then we get back 'Nothing':

     λ> evaluate2 (Number2 12 `Add2` Throw2)
     Nothing
-}

-- maybeApply (Just (+)) :: Maybe Int -> Maybe (Int -> Int)

maybeApply :: Maybe (a -> b) -> Maybe a -> Maybe b
maybeApply (Just f) (Just a) = Just (f a)
maybeApply Nothing  _        = Nothing
maybeApply _        Nothing  = Nothing

maybePure :: a -> Maybe a
maybePure a = Just a


{-     PART III : EVALUATION WITH PRINTING

   Exceptions are a kind of "side-effect" that can happen when we try
   to evaluate an expression. Another possible side effect we might
   have during execution of a program is the printing of logging
   messages. We now extend the 'Expr' type in a different way  to
   include the possibility of printing by adding a constructor 'Print
   message e'. The intended meaning is that this prints the message
   'message' and then executes 'e'. -}

data Expr3
  = Number3 Int
  | Add3    Expr3 Expr3
  | Print3  String Expr3
  deriving Show

{- An example program using this new feature is the following, which
   intersperses some arithmetic with instructions to print out some
   messages: -}

printingProg :: Expr3
printingProg =
  (Print3 "Hello" (Number3 23))
  `Add3`
  (Number3 34 `Add3` (Print3 " World" (Number3 56)))

{- To evaluate expressions with printing, we keep a log of all the
   messages that are printed, in order. We represent this log using a
   list. Therefore, the result type of our evaluator is a pair of the
   string printed, and the resulting integer: -}
evaluate3 :: Expr3 -> (String, Int)
{- Evaluating a number 'n' results in the empty message being printed,
   and the number 'n' as the final answer. -}
evaluate3 (Number3 n)  = ("", n)
{- Evaluating 'Add3 e1 e2' means we must evaluate 'e1', getting the
   output during that evaluation and its integer value, then we
   evaluate 'e2' getting the second output and its integer
   value. Finally, we combine the outputs (using '++') and the
   integers (using '+'). We use a 'where' clause to name the
   intermediate results arising from evaluating 'e1' and 'e2'. -}
evaluate3 (Add3 e1 e2) = (s1 ++ s2, n1 + n2)
  where (s1, n1) = evaluate3 e1
        (s2, n2) = evaluate3 e2
{- Evaluating 'Print3' is where we actually add messages to the output
   -- if we didn't have Print then the only output you can build from
   the empty string and append is the empty string! Printing evaluates
   its second argument to get its result and list of messages, and
   then prepends the new message to the log: -}
evaluate3 (Print3 s e) = (s ++ s1, n)
  where (s1, n) = evaluate3 e

{- Evaluating our test program gives us the messages and result we expect:

       λ> evaluate3 printingProg
       ("Hello World",113)
-}


printApply :: (String, a -> b) -> (String, a) -> (String, b)
printApply (s1, f) (s2, a) = (s1 ++ s2, f a)

printPure :: a -> (String,a)
printPure a = ("",a)

{-
class Functor f => Applicative f where
  pure  :: a -> f a
  (<*>) :: f (a -> b) -> f a -> f b
-}

-- fmap  ::   (a -> b) -> f a -> f b
-- (<*>) :: f (a -> b) -> f a -> f b

evaluateApp :: Applicative f => Expr -> f Int
evaluateApp (Number n)  = pure n
evaluateApp (Add e1 e2) = pure (+) <*> evaluateApp e1 <*> evaluateApp e2


{-     PART IV : EVALUATION WITH CHOICE

   A final side effect we will look at here is non-determinism. To the
   original 'Expr' data type, we add 'Choice' which takes two
   arguments and somehow makes a choice between them. There are
   several different reasonable interpretations of Choice, and we will
   look at two of them. -}

data Expr4
  = Number4 Int
  | Add4    Expr4 Expr4
  | Choice  Expr4 Expr4
  deriving Show

-- if (*) {
--     STATEMENT 1;
-- } else {
--     STATEMENT 2;
-- }

{- Here is an example program that uses 'Choice'. It adds two numbers
   together, but one of those numbers is not fully determined: it
   could either be '0' or '1'. -}

myDitheringProgram :: Expr4
myDitheringProgram =
  ((Number4 0) `Choice` (Number4 1)) `Add4` (Number4 2)

{- Here is a slightly more complex program using 'Choice'. There are now
   two sources of non-determinism within the expression. -}

myDitheringProgram2 :: Expr4
myDitheringProgram2 =
  ((Number4 23 `Choice` Number4 32)
   `Add4`
   ((Number4 34 `Add4` Number4 56)
    `Choice`
    (Number4 23 `Add4` Number4 34)))

-- (23 || 32) + ((34 + 56) || (23 +34))

{- A first attempt at writing an evaluator for expressions with choice
   might have type:

       evaluate4 :: Expr4 -> Int

   However, we get stuck when trying to evaluate (Choice e1 e2):

       evaluate4 (Choice e1 e2) = ???

   We must return a single integer, but we have a choice of two
   expressions to evaluate to get integers! There are several ways out
   of this situation:

     1. We could evaluate both 'e1' and 'e2' and combine their answers
        somehow -- taking their sum, or maximum, or something. This
        seems intuitively to not be faithful to the notion of
        'Choice'.

     2. We could always take 'e1' or always take 'e2'. So we build a
        'biased' interpreter that always makes choices for us. This is
        reasonable, but we could be more general.

     3. We could return all possible choices, perhaps as a list.

     4. We could assume that we are given a supply of booleans that
        tells us how to resolve each choice in turn.

   Options 3 and 4 seem reasonable and interesting, so let's implement
   them.

   We implement Option 3 by writing a function of the following type: -}
evaluate4opt3 :: Expr4 -> [Int]
{- Evaluating a single number has only one possibility, so we return it
   in a list with one element: -}
evaluate4opt3 (Number4 n)  = [n]
{- To evaluate an 'Add', we collect all the possibilities for evaluating
   the two sub-expressions, and then compute all the possible ways of
   adding them together, using a list comprehension. -}
evaluate4opt3 (Add4 e1 e2) = [ n1 + n2 | n1 <- ns1, n2 <- ns2 ]
  where ns1 = evaluate4opt3 e1
        ns2 = evaluate4opt3 e2
{- Evaluation of 'Choice' also collects all the possibilities for
   evaluating its sub-expressions, but then combines them using '++',
   so that we collect all the possible outcomes. -}
evaluate4opt3 (Choice e1 e2) = ns1 ++ ns2
  where ns1 = evaluate4opt3 e1
        ns2 = evaluate4opt3 e2

{- Evaluating 'myDitheringProgram' with 'evalExpr4opt4' now gives us all
   the possible results:

       λ> evaluate4opt3 myDitheringProgram
       [2,3]

   Also for 'myDitheringProgram2', which gives four results arising
   from all combinations of the two choices:

       λ> evaluate4opt3 myDitheringProgram2
       [113,80,122,89]
-}

{- We implement Option 4 by writing a function of the following type: -}
evaluate4opt4 :: Expr4 -> [Bool] -> (Int, [Bool])
{- After we take an Expr4, we take a list of 'Bool's that will tell us
   how to resolve each choice in turn. We then return the integer
   values resulting from evaluating the expression with those choices,
   and the left-over list of choices.

   Evaluating a "pure" number results in just that number, and the
   list of choices is passed through unaffected: -}
evaluate4opt4 (Number4 n) choices =
  (n, choices)
{- Evaluating an addition means that we have to evaluate both
   sub-expressions, but we must be careful to "thread through" the
   list of choices: we evaluate 'e1' with the initial list, getting
   'choices1', which we use to evaluate 'e2', to get 'choices2', which
   we return. -}
evaluate4opt4 (Add4 e1 e2) choices = (n1 + n2, choices2)
  where (n1, choices1) = evaluate4opt4 e1 choices
        (n2, choices2) = evaluate4opt4 e2 choices1
{- Finally, evaluating 'Choice e1 e2' uses one of the choices from the
   list. For simplicity, we assume that we are given enough
   pre-determined choices to evaluate all the 'Choice's in the
   expression, so we don't handle the case with the empty list. -}
evaluate4opt4 (Choice e1 e2) (c:choices) = 
  evaluate4opt4 (if c then e1 else e2) choices

{- NOTE: Notice the similarity between 'evaluate4opt3' and the 'process'
   function from Ex2! -}

{- Evaluating 'myDitheringProgram' with a list of predetermined choices
   yields a single value, and the left-over choices:

       λ> evaluate4opt4 myDitheringProgram [True, True]
       (2,[True])
       λ> evaluate4opt4 myDitheringProgram [False, True]
       (3,[True])

   Evaluating 'myDitheringProgram2' with the same lists consumes all
   the input, due to the presence of two choices in the program:

       λ> evaluate4opt4 myDitheringProgram2 [True, True]
       (113,[])
       λ> evaluate4opt4 myDitheringProgram2 [False, True]
       (122,[])
-}

{- EXERCISE: Implement Option 2. -}

{- EXERCISE: The following Expr5 type also adds the possibilty of
   'Failure' as well as choice. Write extended version of 'evalExpr4'
   and 'evalExpr4opt4' for this new type that also interpret
   Failure. You will have to change the type of 'evalExpr4'. -}

data Expr5
  = Number5 Int
  | Add5    Expr5 Expr5
  | Choice5 Expr5 Expr5
  | Failure5
  deriving Show
