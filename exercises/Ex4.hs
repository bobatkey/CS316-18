{-# OPTIONS_GHC -fwarn-incomplete-patterns #-} 
module Main where

import           Control.Applicative hiding (many)
import           Control.Monad (guard, when)
import           Data.Char (isSpace, isLower, isUpper, isNumber,
                            digitToInt, isAlpha, isAlphaNum)
import           Data.Foldable
import           Data.List (groupBy, intersperse, sortBy)
import qualified Data.Map as M
import           Data.Map (Map)
import           System.Environment (getArgs, getProgName, withArgs)
import           System.Exit (exitFailure)

{----------------------------------------------------------------------}
{- CS316 (2018/19) EXERCISE 4                                         -}
{----------------------------------------------------------------------}

{- Submit by committing to GitLab at or before 2pm on Monday 26th
   November.  There will be a test on this exercise in the lab on that
   date.

   Your combined score from the submission and the test will be worth
   35% of the overall marks for the class (so one mark, below is worth
   half a percent).

   The test will consist of another file which will import this
   file. You will need to answer the questions in that file, and
   commit both by the end of the lab session. -}

{----------------------------------------------------------------------}
{- GHOUL : Global Higher-order Untyped Language                       -}
{----------------------------------------------------------------------}

{- INTRODUCTION TO GHOUL

   This exercise is about building an interpreters for a functional
   language called "GHOUL". It will bring together all of the concepts
   you have learned during this course.

   Here is an example GHOUL program (written using 'unlines' to allow
   multiple lines): -}

plusProgram :: String
plusProgram = unlines
  [ "(plus Z     y) -> y"
  , "(plus (S x) y) -> (S (plus x y))"
  , "(main)         -> (plus (S (S Z)) (S (S Z)))"
  ]

{- Execution of GHOUL programs works by starting from the 'main'
   function and then matching each function application against the
   patterns defined for that function to get an expression to replace
   that application with. This continues until there are no more
   expressions to replace. Data is built from constructors
   (identifiers that start with capital letters) applied to other
   constructors.

   For our example program, we have:

         (main)
      -> (plus (S (S Z)) (S (S Z)))
      -> (S (plus (S Z) (S (S Z))))
      -> (S (S (plus Z (S (S Z)))))
      -> (S (S (S (S Z))))

   So "two plus two is four".

   GHOUL is similar to Haskell, except that there are no types and
   there are no lambda expressions (\x -> ...).

   In this exercise, you will build a GHOUL interpreter, and extend it
   with additional features.

   In general, all interpreters perform the following steps:

     1. Parse the input -- take a representation of a program as a
        list of characters and turn it into abstract syntax. You will
        do this using parser combinators.

     2. Post-process and check the input, often called "Elaboration"
        or "Type Checking". In Haskell this is a complex stage
        involving desugaring, type inference, and typeclass
        resolution. For GHOUL, you will write some code to perform
        some well-formedness checks on the program.

     3. Execute the program. In Lecture 11, we showed you evaluators
        for several very simple languages. GHOUL has three features that
        make evaluation more complex:

          a. Variables
          b. Named function definitions.
          c. Pattern matching.

   These steps are summed up by the function that parses, checks, and
   executes GHOUL programs: -}

runGHOUL :: String -> ErrorOr (ErrorOr Value)
runGHOUL text = do
  prog <- parseAllInput pProgram text
  checkProgram prog
  return (evalProgram prog)


{- 'runGHOUL' accepts a String containing a GHOUL program, parses it,
   elaborates it, checks it, and executes it. If any of these steps
   fail, an error message is returned. Otherwise, the result of
   evaluating the program is returned.

   Errors are tracked by using this datatype. The result of a
   computation is either 'OK x' with some value 'x', or 'Error msg'
   where 'msg' is some hopefully helpful error message. -}

data ErrorOr a
  = OK a
  | Error String
  deriving Show

{- The 'ErrorOr' type is an instance of 'Monad', 'Functor',
   'Applicative' and 'Alternative'. The code implementing these
   interfaces in the appendix below. Apart from these interfaces,
   'ErrorOr' gives us a way of reporting errors: -}

abortWithMessage :: String -> ErrorOr a
abortWithMessage s = Error s

{- Of course, 'runGHOUL' doesn't work yet -- you will need to fill in
   the details below.

   When you've written at least the parser and evaluator parts, you
   should be able to use 'runGHOUL' to run a GHOUL program:

         λ> runGHOUL plusProgram
         OK (OK (VC "S" [VC "S" [VC "S" [VC "S" [VC "Z" []]]]]))

   This exercise is structured so that you can implement a basic GHOUL
   evaluator first, and then go back to extend it with additional
   features for extra marks. As with the previous exercises, roughly a
   third of the marks will only be available during the class test on
   Monday 26th November. -}

{----------------------------------------------------------------------}
{- Part 0 : ABSTRACT SYNTAX                                           -}
{----------------------------------------------------------------------}

{- Before we can write an interpreter for GHOUL programs, we need to
   describe what the syntax of GHOUL programs is. Generalising from
   the 'plus' example above, a GHOUL program is:

     - a list of rules, such as:

           (plus Z y)     -> y
           (plus (S x) y) -> (S (plus x y))
           (main)         -> (plus (S (S Z)) (S (S Z)))

       where:

     - a rule is a name and a list of patterns, surrounded by
       parentheses, followed by an arrow '->' and then an
       expression. For example:

           (plus Z y)     -> y
         or
           (plus (S x) y) -> (S (plus x y))

       where:

     - a pattern is a variable name (first letter lower case) or a
       constructor name (first letter upper case) on its own, or a
       constructor name followed by a space separated list of
       patterns, surrounded by parentheses. Examples:

                 Z
            or   (S Z)
            or   x
            or   (Cons x y)

       and

     - an expression is a variable name, a constructor name, or an
       application of a function to space separated list of
       expressions, surrounded by parentheses, or an application of a
       constructor name to expressions, surrounded by parentheses. For
       example:

                 (S (plus x y))
            or   (plus x y)
            or   Z
            or   x

   Also, there must be a rule named 'main' with no patterns.

   Following this description, we represent GHOUL programs as values
   of the type 'Program', where a 'Program' is a list of rules. -}

type Program = [Rule]

{- As we mentioned above, rules

        (plus Z y) -> y

   consist of:

    1. a name (e.g., "plus", "main")

    2. a list of patterns (patterns are defined below)

    3. an expression for the right hand side (expressions are defined
       below).

   We write a type for representing equations like so: -}

data Rule = MkRule String [Pat] Exp
  deriving (Show, Eq)

{- A pattern is either a variable (PV), or a constructor name and a list
   of patterns (PC). This is similar to patterns in Haskell, except
   that GHOUL does not have a "catch all" pattern '_'. -}

data Pat
  = PV String
  | PC String [Pat]
  deriving (Show, Eq)

{- Here are some example 'Pat's:

   - A GHOUL variable pattern 'y' is represented by the Haskell value
     'PV "y"'.

   - A GHOUL pattern matching a constructor with no arguments,
     e.g. 'Nil', is represented by the Haskell value 'PC "Nil" []'.

   - A GHOUL pattern matching a constructor with some arguments,
     e.g. '(Cons x Nil)', is represented by the Haskell value
     'PC "Cons" [PV "x", PC "Nil" []]'.

   An expression is either a variable (EV), an application of a named
   function (EA) or a an application of a constructor (EC). -}

data Exp
  = EV String
  | EA String [Exp]
  | EC String [Exp]
  deriving (Show, Eq)

{- Here are some example 'Exp's:

   - A GHOUL variable 'x' is represented by the Haskell value
     'EV "x"'.

   - A GHOUL function application '(f x y)' is represented by the
     Haskell value 'EA "f" [EV "x", EV "y"]'.

   - A GHOUL constructor with no arguments, e.g. 'Nil' is represented
     as 'EC "Nil" []'.

   - A GHOUL constructor with some arguments, e.g., '(Cons x Nil)' is
     represented as 'EC "Cons" [EV "x", EC "Nil" []]'.

   Note that in the GHOUL syntax, patterns are a subset of
   expressions, but when we represent them in Haskell they are in two
   separate data types. The job of working out which bits of a GHOUL
   program are patterns and which bits are expressions is done by the
   parser you will define below.

   Here are some example 'Rule's:

   - A GHOUL rule with no arguments:

         (main) -> (plus Z Z)

     is represented by the Haskell value:

         MkRule "main" [] (EA "plus" [EC "Z" [], EC "Z" []])

   - A GHOUL rule with two arguments:

         (eq Z Z) -> True

     is represented by the Haskell value:

         MkRule "eq" [PC "Z" [], PC "Z" []] (EC "True" [])

   Here is an example 'Program', representing the example program
   'plus' we saw above. -}

plusProgramAST :: Program
plusProgramAST =
  [ MkRule "plus"
           [PC "Z" [], PV "y"]
           (EV "y")
  , MkRule "plus"
           [PC "S" [PV "x"], PV "y"]
           (EC "S" [EA "plus" [EV"x", EV"y"]])
  , MkRule "main"
           []
           (EA "plus" [EC "S" [EC "S" [EC "Z" []]],
                       EC "S" [EC "S" [EC "Z" []]]])
  ]

{- It is worth spending time to understand the correspondence between
   the Haskell value 'plusProgramAST' and the concrete syntax in the
   'plusProgram' variable defined above. -}


{- 4.0.0 Write a GHOUL program to concatenate lists.

   Write a GHOUL program to concatenate (append) two lists as a value
   of type 'Program'. Remember that to be a valid GHOUL program, you
   should also include a 'main' function definition. Use the example
   of the 'append' program written in Haskell given in Lecture 01 as a
   guide. -}

appendProgramAST :: Program
appendProgramAST = undefined

{- 3 MARKS -}

{----------------------------------------------------------------------}
{- Part 1 : VALUES and ENVIRONMENTS                                   -}
{----------------------------------------------------------------------}

{- Execution of GHOUL programs proceeds by matching patterns in the
   rules against runtime values. Runtime values are the results of
   executing 'Exp'ressions.

   Runtime values are a subset of 'Exp's, restricted to just
   constructors and constructors applied to other constructors: -}

data Value
  = VC String [Value]
  deriving (Eq, Show)

{- For example, the expression (S Z) evaluates to the 'Value':

      VC "S" [VC "Z" []]
-}

{- 4.1.0 Implement a "pretty printer" for GHOUL values, that prints them
   out in the same syntax as an expression that would generate them. -}

ppValue :: Value -> String
ppValue = undefined

{- HINT: you will find the functions 'intersperse' and 'concat' useful. -}

{- 2 MARKS -}
                           
{- GHOUL programs have variables in them. To keep track of what each
   variable means by during execution, we use environments. An
   environment is Map, associating values to names: -}

type Env = Map String Value

{- The 'Map' data type is provided by the Data.Map module imported at
   the start of this file. The 'Map' type takes two arguments: the
   first is the type of keys (here 'String's representing names), and
   the second is the type of values (here 'Value', representing GHOUL
   values).

   The Data.Map module provides a rich interface for querying and
   updating 'Map's. The module has been imported as 'qualified M',
   meaning that the functions from this module need to be prefixed by
   'M.' to be used. This is to avoid name clashes.

   For this exercise, you will need the following functions that build
   and manipulate 'Map's (use ':t <functioname>' in GHCi to look at
   the types as well):

      M.empty           -- the empty Map

      M.lookup k m      -- looks up the value of the key 'k' in 'm'.
                           Returns 'Just v' if the value 'v' is
                           associated with 'k' in 'm'. Otherwise,
                           returns 'Nothing'.

      M.insert k v m    -- returns a new map that has the same key->value
                           mapping as 'm' except that now 'k' has the
                           value 'v'.

      M.fromList kvs    -- constructs a 'Map' from a list 'kvs' of
                           (key,value) pairs. If a key is repeated, then
                           the last value is taken.

   For example, the empty environment (no variables have values) is the
   empty map:

        M.empty
     or
        M.fromList []

   An environment that assigns the GHOUL value 'Z' to 'x' and '(S Z)'
   to 'y' is the list:

        M.insert "x" (VC "Z" []) (M.insert "y" (VC "S" [VC "Z" []]) M.empty)
     or
        M.fromList [ ("x", VC "Z" [])
                   , ("y", VC "S" [VC "Z" []])
                   ]

    We look up the values assigned to variables in an environment
    using the 'M.lookup' function:

       > let env = M.fromList [ ("x", VC "Z" []), ("y", VC "S" [VC "Z" []]) ]
       > M.lookup "x" env
       Just (VC "Z" [])
       > M.lookup "z" env
       Nothing
       > M.lookup "z" (M.insert "z" (VC "Nil" []) env)
       Just (VC "Nil" [])
-}

{- 4.1.1 Binding Variables

   Implement the function 'bindVar' that acts like 'M.insert' except
   that a call:

          bindVar x value env

   acts as follows:

     - if the variable 'x' is already in the environment, then it
       returns 'Error' with a suitable error message.

     - if the variable 'x' is not in the environment, it inserts it
       into 'env' to get 'newEnv' and returns 'OK newEnv'.

   Examples:

         bindVar "x" (VC "Z" []) M.empty
      == OK (M.fromList [("x", VC "Z" [])])

         bindVar "x" (VC "Z" []) (M.insert "x" (VC "Nil" []) M.empty)
      == Error <error message>
-}

bindVar :: String -> Value -> Env -> ErrorOr Env
bindVar x value env = undefined

{- 2 MARKS -}

{----------------------------------------------------------------------}
{- Part 2 : PATTERN MATCHING                                          -}
{----------------------------------------------------------------------}

{- Execution of a GHOUL program alternates between choosing which rule
   to use, and then evaluating the right-hand side of that
   rule. Choosing rules is accomplished by pattern matching. Matching
   a pattern against a value updates an environment to bind variables
   in the pattern to values. For example (using GHOUL syntax for
   brevity):

    - Matching the pattern 'x' against any value 'v' will bind 'x' to
      'v'.

    - Matching the pattern '(S x)' against the value '(S Z)' will bind
      'x' to the value 'Z'.

    - Matching the pattern '(S x)' against the value 'Z' will fail,
      because the constructors 'S' and 'Z' do not match.

   We will be matching lists of patterns against lists of values. A
   tempting way to pair up patterns with values for matching is to use
   the 'zip' function from the Haskell library. However, if the input
   lists are different lengths, then the standard 'zip' ignores all
   the extra elements in the longer list. So we define our own
   'zipChecked' function that uses a 'Maybe' to signal when the lists
   are different lengths. -}

zipChecked :: [a] -> [b] -> ErrorOr [(a,b)]
zipChecked xs ys = go xs ys []
  where go []     []     zs = return (reverse zs)
        go (x:xs) (y:ys) zs = go xs ys ((x,y):zs)
        go _      _      _  = abortWithMessage "lists of different lengths"


{- 4.2.0 Matching patterns and lists of patterns.

         Write the functions 'matchPattern' and 'matchPatterns'.

   'matchPattern' takes a pair of a pattern and a value and an
   environment and returns a possibly updated environment. This ought
   to implement pattern matching:

     - matching a variable against any value binds the variable to the
       value (using 'bindVar').

     - matching a constructor pattern against a value checks that the
       value has a constructor with the same name, and that the
       sub-patterns match all of the sub-values.

   'matchPats' should take a list of patterns and a list of values,
   and return a Matcher generated by matching all the pairs. If the
   two lists have different lengths, then matching ought to fail. Use
   the 'zipChecked' function we gave you above, which checks that two
   lists have the same length.

   FIXME: some test cases.
-}

matchPattern :: (Pat, Value) -> Env -> ErrorOr Env
matchPattern = undefined

matchPatterns :: [(Pat,Value)] -> Env -> ErrorOr Env
matchPatterns = undefined

{- 4 MARKS -}

{- 4.2.1 Finding Rules by Pattern Matching

   Write a function that, given a name and a list of values, searches
   a 'Program' for the first rule that matches. That is, the names
   should match, and the patterns of the equation should match the
   list of values. On success, you should return the expression
   associated with that rule.

   One way to write this function is to use the 'Alternative'
   typeclass functions 'empty' and '<|>' to represent failure to find
   a rule and ordered choice, respectively.

   FIXME: some test cases.
-}

findRule :: String -> [Value] -> Program -> Env -> ErrorOr (Env, Exp)
findRule = undefined

{- 4 MARKS -}

{----------------------------------------------------------------------}
{- 4.2.2 WILL APPEAR IN THE TEST                                      -}
{- 3 MARKS -}
{----------------------------------------------------------------------}

{----------------------------------------------------------------------}
{- Part 3: EVALUATION OF EXPRESSIONS                                  -}
{----------------------------------------------------------------------}

{- Evaluation of expressions in the context of some program is modelled
   using the 'Eval' data type. The 'Eval' type offers two services as
   well as being a 'Monad':

     a) The ability to look at the current program ('currentProgram')

     b) The ability to report failed execution ('abortEval'). -}

newtype Eval a =
  MkEval (Program -> ErrorOr a)

{- To 'run' some evaluation, we use the 'runEval' function that runs an
   evaluation with a given program and environment: -}

runEval :: Eval a -> Program -> ErrorOr a
runEval (MkEval e) program = e program

{- 'Eval' supports the Monad operations 'return' and '>>=', which should
   not be surprising since it is the combination of the 'Reader' monad
   (Lecture 16), and the 'ErrorOr' monad. As a consequence it also
   supports the 'Functor' and 'Applicative' interfaces: -}

instance Monad Eval where
  return x = MkEval (\prg -> return x)

  e >>= k = MkEval (\prg -> do a <- runEval e prg
                               runEval (k a) prg)

instance Functor Eval where
  fmap f ea = do a <- ea; return (f a)

instance Applicative Eval where
  pure = return
  ef <*> ea = do f <- ef; a <- ea; return (f a)

{- The three basic operations supported by the 'Eval' monad are the ones
   that abort evaluation with an error message ('abortEval'), access
   the program being executed ('currentProgram'), and lift a
   computation that may error (an 'ErrorOr' computation) up to a
   'Eval' computation. -}

abortEval :: String -> Eval a
abortEval msg = MkEval (\prg -> abortWithMessage msg)

currentProgram :: Eval Program
currentProgram = MkEval (\prg -> return prg)

liftError :: ErrorOr a -> Eval a
liftError e = MkEval (\prg -> e)

{- 4.3.0 Expression evaluation

   Write the 'eval' function. This function takes two arguments:

     - The 'Env'ironment that describes what values are assigned to
       what variables.

     - The 'Exp'ression to evaluate.

   It returns a computation in the 'Eval' monad.

   Evaluation proceeds like so:

     - If the expression is a variable 'EV var', then look up that
       variable in the environment. If it is not there then abort with
       an appropriate error message.

     - If the expression is a constructor 'EC c args', then evaluate
       all the arguments (you might find the 'traverse' function
       useful here). Then return 'VC c' applied to the result of
       evaluating all the 'args'.

     - If the expression is a function application 'EA f args', first
       evaluate all the arguments to values. Then get the current
       program and use 'findRule' to find a rule that matches the
       values computed from the arguments and the updated
       environment. Then evaluate the right hand side of that rule in
       the environment generated by pattern matching.

   FIXME: test cases.
-}

eval :: Env -> Exp -> Eval Value
eval = undefined

{- 5 MARKS -}

{- Once you have implemented 'eval', the following function to evaluate
   whole programs will work. This is used by runGHOUL. -}

evalProgram :: Program -> ErrorOr Value
evalProgram prog =
  runEval                    -- run an evaluation
    (eval M.empty (EA "main" [])) -- of the expression that calls the 'main' function
    prog                     -- in the given program

{----------------------------------------------------------------------}
{- 4.3.1 WILL APPEAR IN THE TEST                                      -}
{- 10 MARKS -}
{----------------------------------------------------------------------}

{----------------------------------------------------------------------}
{- Part 4 : PARSING                                                   -}
{----------------------------------------------------------------------}

{- Writing GHOUL programs as values of type 'Program' is all very well,
   but not very friendly. Instead, we will build a parser and
   elaborator that will take a String that represents a GHOUL program
   and turn it into a list of equations. A list of equations is not
   yet a program, so Part 2 will build an elaborator to convert lists
   of equations into proper 'Program's.

   You will build your parser using parser combinators, as introduced
   in Lecture 14. Unlike in Lecture 14, we will write parsers that
   produce error messages (see the 'Either' monad in Lecture 13),
   rather than just returning 'Nothing' on failure. -}

newtype Parser a = MkParser (String -> ErrorOr (a, String))

{- Parsers are applied to 'String's by using the 'runParser' function,
   which returns the value parsed, and the left over input: -}

runParser :: Parser a -> String -> ErrorOr (a,String)
runParser (MkParser p) input = p input

{- To parse a complete string all the way to the end, we use
   'parseAllInput', which checks that the end of the string has been
   reached using the 'eoi' parser. -}

parseAllInput :: Parser a -> String -> ErrorOr a
parseAllInput p input =
  do (a, _) <- runParser (p <* eoi) input
     return a

{- The rest of the parser combinator functions are at the end of this
   file. The main combinators that you will want to use to build your
   parsers are:

     - The Functor, Applicative, Monad, and Alternative interfaces
     - 'isChar' to parse given characters
     - 'string' is parse given strings
     - 'identifier' to parse identifiers: sequences of letters and numbers
       that must start with a letter.
     - 'spaces' to parse zero or more white space characters.
     - 'sepBy' to parse lists of things separated by something.

   To begin the GHOUL parser, you will first construct two parsers
   that recognise variable names and constructor names. We will use
   these later on as part of our pattern and expression parsers. -}

{- 4.4.0 Write a 'Parser' for 'variable names'.

   Follow the Haskell convention that a variable name is an identifier
   that starts with a lower case letter. Use the library function
   'isLower' to identify lower case letters. -}

varname :: Parser String
varname = undefined

{- Here are some tests that your 'varname' parser should pass:

     runParser varname "plus"  == Right ("plus", "")
     runParser varname "x"     == Right ("x", "")
     runParser varname "Plus"  == Left <error message>
     runParser varname ""      == Left <error message>
     runParser varname "plu s" == Right ("plu", " s")
     runParser varname "123"   == Left <error message>

  Note that the tests do not specify what error messages look
  like. That is up to you. -}

{- 1 MARK -}


{- 4.4.1 Write a 'Parser' for 'constructor names'.

   Follow the convention that a constructor name is an identifier that
   starts with an upper case letter. Use the library function
   'isUpper' to identify upper case letters. -}

constructorname :: Parser String
constructorname = undefined

{- Here are some tests that your 'constructorname' parser should pass:

     runParser constructorname "plus"  == Left <error message>
     runParser constructorname "x"     == Left <error message>
     runParser constructorname ""      == Left <error message>
     runParser constructorname "Plus"  == Right ("Plus", "")
     runParser constructorname "S"     == Right ("S", "")
     runParser constructorname "plu s" == Left <error message>
     runParser constructorname "123"   == Left <error message> -}

{- 1 MARK -}


{- 4.4.2 Parsing patterns.

   A pattern is either:

     - a variable name; or
     - a constructor name followed by a whitespace separated list of patterns,
       all surrounded by parentheses; or
     - a constructor name.

   For example:

         (Cons Z xs)

   Write a parser for patterns, using the parser combinators. -}

pPat :: Parser Pat
pPat = undefined

{- Here are some tests that your 'pat' parser should pass:

     runParser pPat "x"       == OK (PV "x","")
     runParser pPat "Z"       == OK (PC "Z" [],"")
     runParser pPat "(S x)"   == OK (PC "S" [PV "x"],"")
     runParser pPat "(S x y)" == OK (PC "S" [PV "x",PV "y"],"")
     runParser pPat ""        == Error <error message>
     runParser pPat "S x"     == OK (PC "S" []," x")
     runParser pPat "x(x,y)"  == OK (PV "x","(x,y)")

   Note the last two cases: they have only parsed part of the input,
   and returned the bit they couldn't parse. -}

{- 3 MARKS -}


{- 4.4.3 Parsing expressions

   An expression is either:

     - a variable name followed by a space separated list of
       expressions, all in parentheses, which is interpreted
       as a function call; or
     - a constructor name followed by a space separated list of
       expressions, all in parentheses; or
     - a variable name; or
     - a constructor name.

   For example:

        (append (Cons Z Nil) xs)

   Write a parser for expressions. This will be very similar to the
   parser for patterns above, so it is worth fewer marks. -}

pExp :: Parser Exp
pExp = undefined

{- FIXME: some test cases -}

{- 2 MARKS -}

{- 4.4.4 Parsing Rules.

   The concrete syntax for rules looks like:

      (plus (S x) y) -> (S (plus x y))

   Using the 'pat' and 'expr' parsers you wrote above, write a
   'Parser' for equations. To be programmer friendly, you should be
   flexible about spaces, using the 'spaces' parser.

   FIXME: test cases -}

pRule :: Parser Rule
pRule = undefined

{- 3 MARKS -}

{- 4.4.5 Parsing lists of Rules, aka Programs.

   The final stage of parsing is a parser for lists of equations,
   separated by zero or more spaces. You should also allow for spaces
   at the beginning and end of the input too. -}

pProgram :: Parser Program
pProgram = undefined

{- 2 MARKS -}

{- Once you have implemented the parser and evaluator, you will be able
   to run GHOUL programs. FIXME: more detail. -}

{----------------------------------------------------------------------}
{- 4.4.6 WILL APPEAR IN THE TEST                                      -}
{- 5 MARKS -}
{----------------------------------------------------------------------}



{----------------------------------------------------------------------}
{- Part 5 : CHECKING                                                  -}
{----------------------------------------------------------------------}

{- The GHOUL interpreter that you wrote above tries its best with any
   'Program' that it is given, but there are some silly mistakes that
   programmers can make that can be relatively easily checked before
   execution.

   In this part, you will write two checks on GHOUL programs that
   check for mistakes:

     - Not having a 'main' function, or the main function taking
       arguments. (Question 4.5.0 below).

     - Using variables on the right-hand side of an equation that are
       not mentioned in the pattern on the left-hand side (Question
       4.5.1, below). For example:

          (plus Z x) -> y

       This equation will always fail during execution, because there
       is no value for 'y'.

  The following function runs the two checks listed above. However,
  all of the functions that actually do the checking just immediately
  return successfully. It is your task to fill them in. -}

checkProgram :: Program -> ErrorOr ()
checkProgram prog = do
  hasMain prog
  scopeCheck prog

{- In the functions below, you can use the 'abortWithMessage' function
   defined above to report errors in the ErrorOr monad. -}

{- 4.5.0 Checking for a main function

   Write a function that checks a 'Program' for a rule for a function
   called 'main' that has no arguments. If it doesn't, then you should
   return a useful error message. FIXME: what there are multiple such?

   FIXME: test cases.
-}

hasMain :: Program -> ErrorOr ()
hasMain = undefined

{- 1 MARK -}


{- 4.5.1 Scope checking

   Write a function that checks each equation is "well-scoped". This
   means that all the variables mentioned on the right-hand side (in
   the 'Exp') are mentioned on the left-hand side (in the
   patterns). You may find it helpful to write a function that checks
   individual rules first, and then use 'traverse_' to check every
   rule in a program. -}

scopeCheck :: Program -> ErrorOr ()
scopeCheck prog = undefined

{- 5 MARKS -}

{----------------------------------------------------------------------}
{- 4.5.2 WILL APPEAR IN THE TEST                                      -}
{- 4 MARKS -}
{----------------------------------------------------------------------}

{----------------------------------------------------------------------}
{- Part 6 : ADDITIONAL FEATURE                                        -}
{----------------------------------------------------------------------}

{- 4.6.0 Built-in arithmetic

   As it stands, GHOUL is reasonably expressive but not very
   efficient. One source of inefficency is the fact that numbers are
   represented as sequences of 'S's followed by a 'Z'. This makes
   almost all operations on numbers take time linearly proportional to
   the size of the number, not to mention the wasted memory space.

   A more efficient approach would be to use Haskell's built in
   integer arithmetic for numbers. In this question, you should extend
   the GHOUL system so that it has support for values that are
   represented by Haskell values of type 'Int'.

   You will need to:

     1. Extend the type of 'Exp'ressions so that they can include 'Int'
        constants.

     2. Extend the type of 'Value's so that values can be 'Int's as
        well as constructors applied to values. You'll also have to
        extend 'ppValue'.

     3. Extend expression evaluation so that special arithmetic
        functions are recognised and specially handled. At least you
        should implement 'add' that adds two 'Int' values, and 'eq'
        that compares two 'Int's for equality and returns 'True' if
        they are equal and 'False' otherwise.

     4. Extend the parser to allow for 'Int' constants (use the
        'number' parser defined below).

     5. (Optional) extend the pattern matcher so that it allows
        matching 'Int's as well a constructors.

   Summarise the changes you make here, so that we know what you did: -}

{- 10 MARKS -}

{----------------------------------------------------------------------}
{- APPENDIX : RUNNING GHOUL FROM THE COMMAND LINE                     -}
{----------------------------------------------------------------------}

{- The following main function makes it possible to compile this file
   and run your GHOUL evaluator from the command-line, reading a
   source program from a file name given as an argument. (Of course,
   this won't work until you have managed to get runGHOUL working!)

   You can compile your file as follows:

         $ ghc Ex4.hs
         [1 of 1] Compiling Main             ( Ex4.hs, Ex4.o )
         Linking Ex4 ...

   This will produce an executable file named 'Ex4' in the current
   directory. To run it on a file plus.ghoul containing a GHOUL
   program, execute it like this:

         $ Ex4 plus.ghoul
         (S (S (S (S Z))))

   We will talk more about compiling Haskell programs to executables
   in Lecture 18.

   You can also run the main function with a "faked" commandline
   argument from ghci directly, without compiling, by running

         λ> withArgs ["plus.ghoul"] main
   -}

main :: IO ()
main = do
  args <- getArgs
  progName <- getProgName
  ghoulFile <-
    case args of
      [f] -> return f
      _   -> exitFail ("not exactly one input file.\n" ++
                       "Usage: " ++ progName ++ " <input-file>")
  input <- readFile ghoulFile
  case runGHOUL input of
    Error err      -> exitFail err
    OK (Error err) -> exitFail err
    OK (OK v)      -> putStrLn (ppValue v)
  where
    exitFail msg = do putStrLn ("GHOUL: " ++ msg)
                      exitFailure

{----------------------------------------------------------------------}
{- APPENDIX : ERROR PLUMBING                                          -}
{----------------------------------------------------------------------}

{- Here is the code that implements the Monad, Functor, Applicative and
   Alternative interfaces for the 'ErrorOr' type. -}

instance Monad ErrorOr where
  return x = OK x
  OK a    >>= f = f a
  Error s >>= _ = Error s

instance Functor ErrorOr where
  fmap f (OK a) = OK (f a)
  fmap f (Error s) = Error s

instance Applicative ErrorOr where
  pure = return
  OK f    <*> OK a    = OK (f a)
  Error s <*> _       = Error s
  _       <*> Error s = Error s

instance Alternative ErrorOr where
  empty = Error "<empty>"
  OK a    <|> _       = OK a
  _       <|> OK a    = OK a
  Error s <|> _       = Error s

{----------------------------------------------------------------------}
{- APPENDIX : PARSER COMBINATORS                                      -}
{----------------------------------------------------------------------}

{- Here is the code for the parser combinators you should use to
   implement your GHOUL parser. You may want to consult this code to
   help you write your parser, but do not alter it. -}

instance Functor Parser where
  fmap f (MkParser p) =
    MkParser (fmap (fmap (\(a,s) -> (f a,s))) p)

instance Applicative Parser where
  pure x = MkParser (\s -> return (x,s))

  MkParser pf <*> MkParser pa =
    MkParser (\s -> do (f, s1) <- pf s
                       (a, s2) <- pa s1
                       return (f a, s2))

instance Monad Parser where
  MkParser p >>= k =
    MkParser (\s -> do (a, s1) <- p s
                       let MkParser p2 = k a
                       p2 s1)

instance Alternative Parser where
  empty = MkParser (\s -> empty)

  MkParser p1 <|> MkParser p2 =
    MkParser (\s -> p1 s <|> p2 s)

eoi :: Parser ()
eoi = MkParser (\s -> case s of
                        "" -> return ((), "")
                        s  -> abortWithMessage ("expecting end of input; got " ++ show s))

parseFail :: String -> Parser a
parseFail msg = MkParser (\s -> abortWithMessage msg)

char :: Parser Char
char = MkParser p
  where p []     = abortWithMessage "expecting a character, but end of input was found"
        p (c:cs) = return (c, cs)

isChar :: Char -> Parser ()
isChar expected = do
  seen <- char
  if expected == seen then
    return ()
  else
    parseFail ("Expecting " ++ show expected ++ ", got " ++ show seen)

satisfies :: String -> (Char -> Bool) -> Parser Char
satisfies p_description p = do
  c <- char
  if p c then return c else parseFail ("Expecting " ++ p_description ++ ", got " ++ show c)

string :: String -> Parser ()
string = mapM_ isChar

digit :: Parser Int
digit = do
  c <- char
  if isNumber c then
    return (digitToInt c)
  else
    parseFail "Expecting a digit"

number :: Parser Int
number = foldl (\l r -> l*10+r) 0 <$> some digit

space :: Parser ()
space = () <$ satisfies "a space character" isSpace

spaces :: Parser ()
spaces = () <$ many space

identifier :: Parser String
identifier = (:) <$> satisfies "alphabetic character" isAlpha
                 <*> many (satisfies "alphanumeric character" isAlphaNum)

sepBy :: Parser () -> Parser a -> Parser [a]
sepBy sep p = (:) <$> p <*> many (sep *> p) <|> pure []

many :: Parser a -> Parser [a]
many p = (:) <$> p <*> many p <|> pure []
