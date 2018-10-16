module Lec08 where

{-             LECTURE 08 : RECURSION SCHEMES

   In Lecture 5, we covered how to define recursive functions --
   functions that are defined in terms of themselves. Recursion is the
   way that we write functional programs that compute with structures
   whose size is unknown at the time of writing the program.

   After writing a few recursive functions, we can see that they often
   fall into a few standard "patterns". The pattern we will look at in
   this lecture are recursive functions that systematically replace
   the constructors of a recursive data type with values and
   functions, building up a new value from a value of the
   datatype. This pattern is called 'iteration' over the data type (it
   is also sometimes called 'fold', but we reserve this for Foldable
   we will see in Lecture 09). Iteration turns out to be surprisingly
   expressive. -}

{- For the first example, we will use the type of natural numbers,
   defined recursively in terms of 'Zero' and 'Succ': -}

data Nat
  = Zero
  | Succ Nat
  deriving Show

{- Values of type 'Nat' are built by using the constructors. We can
   check the types of the constructors by using GHCi:

     λ> :t Zero
     Zero :: Nat
     λ> :t Succ
     Succ :: Nat -> Nat

   The idea is that 'Zero' represents '0' and 'Succ' represents
   '+1'. So we can represent any positive whole number by starting
   from Zero and using Succ as many times as we need.

   Here are two example values of type 'Nat': 'one' and 'two'. -}

one = Succ Zero
two = Succ one

{- Addition of 'Nat's can be defined by the following recursively
   defined function. We will look at the structure of this function to
   see the underlying 'pattern' of recursion that is being used: -}

plus :: Nat -> Nat -> Nat
plus Zero     n = n
plus (Succ m) n = Succ (plus m n)

{- Looking at 'plus' we can see that 'n' is left unchanged thoughout, so
   we can rewrite it to make this clearer: -}

plus' :: Nat -> Nat -> Nat
plus' m n = plusHelper m
  where plusHelper Zero     = n
        plusHelper (Succ m) = Succ (plusHelper m)

{- Looking at 'plusHelper', we can make two observations:

   1. There is a line for each constructor of the 'Nat' type,
      declaring what to do for each constructor.

   2. The recursive call to 'plusHelper' in the 'Succ' case is on 'm'.
      The function doesn't look at the value of 'm' directly, it only
      uses the value recursively generated from it.

   We can summarise 'plusHelper' by what it does on the two
   constructors: On Zero, it returns 'n'. On (Succ m) it applies Succ
   to the result of recursively processing 'm'.

   This pattern of returning a value for 'Zero' and applying a
   function for 'Succ' is very common, and we term this process
   'iteration'. Iteration for natural numbers of expressed by the
   'iterNat' function: -}

iterNat :: a -> (a -> a) -> Nat -> a
iterNat zero succ Zero     = zero
iterNat zero succ (Succ n) = succ (iterNat zero succ n)

{- The type of 'iterNat' states:

     iterNat :: a        -- a value to use for 'Zero'
             -> (a -> a) -- a function to use for 'Succ'
             -> Nat      -- a Nat to look at
             -> a        -- the value returned by looking at the Nat

   See? We are systematically replacing the constructors in any value
   of 'Nat' with the value and argument provided.

   To see how to use 'iterNat', let's write 'plus' using it: -}

plus2 :: Nat -> Nat -> Nat
plus2 m n = iterNat -- 'a = Nat'
               n -- zero case
               Succ -- (\plus2_m_n -> Succ plus2_m_n) -- succ case
               m

{- To use 'iterNat', we must provide the 'Zero' and 'Succ'
   cases. Following the discussion above, we use 'n' for the 'Zero'
   case, and 'Succ' for the 'Succ' case.

   Another way to write 'plus' using 'iterNat' is to pass around 'n'
   each time, just as we did in the original definition of 'plus'. We
   accomplish this by using 'iterNat' to build a function 'Nat -> Nat'
   instead of to build a 'Nat'. This means that we are using 'iterNat'
   with type 'a = Nat -> Nat': -}

plus3 :: Nat -> (Nat -> Nat)
plus3 m = iterNat -- 'a = Nat -> Nat'
             id -- zero case
             (\plus3_m n -> Succ (plus3_m n)) -- succ case
             m

{- In 'plus3', the 'Zero' case is the identity function 'id', which just
   takes 'n' and returns 'n'. In the 'Succ' case, we are given the
   result of computing 'plus3_m' and we are given 'n', so we use
   'plus3_m n' to get the result of adding 'm' to 'n', and then apply
   'Succ'.

   Passing 'n' around gives us some more flexibility. Here is a
   version of plus that modifies 'n' as it goes, adding one to 'n' for
   every 'Succ' discovered in 'm': -}

plus4 :: Nat -> Nat -> Nat
plus4 m = iterNat -- 'a = Nat -> Nat'
             id -- zero
             (\plus4_m n -> plus4_m (Succ n)) -- succ
             m

{- On natural numbers 'plus3' and 'plus4' are equivalent, because one
   'Succ' looks like every other 'Succ'. However, if we attached
   values to the 'Succ's (e.g. as we do in lists), then they would
   have different behaviour. -}

{- The following function is an interesting special case. What happens
   when we use 'Zero' as the value for 'Zero', and 'Succ' as the
   function for 'Succ'? -}

thingy :: Nat -> Nat
thingy = iterNat Zero Succ

{- We get the identity function! 'thingy x' is always equal to
   'x'. Replacing each constructor with itself gives us back the
   original value. This may seem like just a useless way to compute
   nothing, but this technique will be useful for keeping track of
   where we are in a recursive computation. -}

{- Let's look at another example of a function on 'Nat's: the equality
   testing function.

   We can start to define this function using 'iterNat' as follows: -}

eqNat0 :: Nat -> Nat -> Bool
eqNat0 = iterNat -- 'a = Nat -> Bool'
           undefined -- need an 'is this zero?' test
           (\eqNat_m n -> undefined) -- need to determine whether Succ
                                     -- m = n, given a function that
                                     -- can answer is 'x' equal to
                                     -- 'm'?

{- There are two holes left in the definition. To fill in the first one
   we need to write a function that determines whether a 'Nat' is
   'Zero'. We can do this with a 'case' expression: -}

eqNat1 :: Nat -> Nat -> Bool
eqNat1 = iterNat -- 'a = Nat -> Bool'
           (\n -> case n of
                    Zero -> True
                    Succ _ -> False)
           (\eqNat_m n -> undefined)

{- To fill in the second hole, we need a function that can answer "is
   Succ m = n", given a test that can answer "is m = x", for any
   "x". Thinking a bit, we can see that "Succ m = n" is only true if
   "n = Succ n'" for some n'. So we use a 'case' expression again: -}

eqNat2 :: Nat -> Nat -> Bool
eqNat2 = iterNat -- 'a = Nat -> Bool'
           (\n -> case n of
                    Zero -> True
                    Succ _ -> False)
           (\eqNat_m n -> case n of
                            Zero -> False
                            Succ n' -> eqNat_m n')

{- Let's step through a run of 'eqNat2' to get a feel for what is going
   on. We write

     zeroCase = (\n -> case n of
                         Zero -> True
                         Succ _ -> False)

     succCase = (\eqNat_m n -> case n of
                                 Zero -> False
                                 Succ n' -> eqNat_m n')

     so that eqNat2 = iterNat zeroCase succCase.

     eqNat2 (Succ Zero) (Succ (Succ Zero))
   =   { write superfluous brackets for emphasis }
     (eqNat2 (Succ Zero)) (Succ (Succ Zero))
   =   { expand definition of eqNat2 }
     (iterNat zeroCase succCase (Succ Zero)) (Succ (Succ Zero))
   =   { definition of iterNat ... (Suc Zero) }
     (succCase (iterNat zeroCase succCase Zero)) (Succ (Succ Zero))
   =   { expand definition of succCase }
     (\eqNat_m n -> case n of Zero -> False; Succ n' -> eqNat_m n')
      (iterNat zeroCase succCase Zero)
      (Succ (Succ Zero))
   =   { application of a lambda expression to arguments }
     case (Suc (Suc Zero)) of
       Zero -> False
       Succ n' -> (iterNat zeroCase succCase Zero) n'
   =   { case expression of a constructor }
     (iterNat zeroCase succCase Zero) (Suc Zero)
   =   { definition of iterNat ... Zero }
     zeroCase (Suc Zero)
   =   { expand definition of zeroCase }
     (\n -> case n of Zero -> True; Succ _ -> False) (Suc Zero)
   =   { application of a lambda expression to arguments }
     case (Suc Zero) of Zero -> True; Succ _ -> False
   =   { case expression of a constructor }
     False

   So 'one' is not equal to 'two'. Try stepping through
   eqNat2 (Suc Zero) (Suc Zero) yourself!
-}


{- Explicitly pattern matching seems to go against the spirit of
   'iterNat'. Can we replace the 'case' expressions with uses of
   'iterNat'?

   For the 'Zero' case, where we need a 'is Zero' test, this is
   possible. To identify 'Zero's, we replace every 'Zero' with 'True'
   and every 'Succ' with the constantly 'False' function: -}

eqNat3 :: Nat -> Nat -> Bool
eqNat3 = iterNat -- 'a = Nat -> Bool'
           (iterNat True (\_ -> False))
           (\eqNat_m n -> case n of
                            Zero -> False
                            Succ n' -> eqNat_m n')

{- The 'Succ' case is more difficult. If we break it out into its own
   function, we can see the problem. -}

succCase :: (Nat -> Bool) -> Nat -> Bool
succCase eqNat_m = iterNat False (\succCase_eqNat_m_n -> undefined)

{- To fill in the 'undefined' part, we have the following task. Given a
   number 'Succ n', and the result of 'eqNat_m_n' we want to know
   whether 'm' is equal to 'Succ n'. Working this out from the
   available information is impossible!

   The problem is that 'iterNat' doesn't give us the 'n' from the
   'Succ n', only the result of recursively processing it. We seem to
   need a new kind of recursion scheme to handle this case. We define
   'caseNat' to capture the pattern being used in the 'case'
   expressions: -}

caseNat :: a -> (Nat -> a) -> Nat -> a
caseNat zero succ Zero = zero
caseNat zero succ (Succ k) = succ k

{- 'caseNat' is similar to 'iterNat', except that it does not call
   itself recursively. The 'Nat' 'k' is passed directly into the
   'succ' function.

   Using 'caseNat', we can write 'eqNat' without explicit recursion or
   pattern matching: -}

eqNat :: Nat -> Nat -> Bool
eqNat = iterNat -- 'a = Nat -> Bool'
        (iterNat True (\eq_Zero_k -> False))
        (\ eqNat_m -> caseNat False
                              (\ k -> eqNat_m k))

{- The existence of 'iterNat' and 'caseNat' is unsatisfying. Is there a
   recursion scheme that gives us access to both the recursive result
   and the value being examined?

   'recNat' is a recursion scheme that does this: -}

recNat :: a -> ((Nat,a) -> a) -> Nat -> a
recNat zero succ Zero     = zero
recNat zero succ (Succ n) = succ (n, recNat zero succ n)

{- Compared to 'caseNat', 'recNat' calls itself recursively. Compared to
   'iterNat', the 'Succ n' case passes 'n' to the 'succ' function.

   Because it passes more information to 'succ', 'recNat' appears to
   be more powerful than 'iterNat'. However, this extra power is
   illusory because we can implement 'recNat' from 'iterNat' by
   building a copy of the 'Nat' we are processing. This uses the same
   technique as 'thingy' above.

   The tricky to defining 'recNat' is to use 'iterNat' to compute a
   pair (n,b) consisting of: 'n', a copy of the natural being
   processes; and 'b' the result required. At the end of the
   computation, we use 'snd' to get the final result, and discard the
   'Nat', which was only needed for intermediate computations. -}

recNatFromIterNat :: b -> ((Nat,b) -> b) -> Nat -> b
recNatFromIterNat zero succ n
  = snd (iterNat -- 'a = (Nat, b)'
          (Zero, zero)
          (\(n, rec_n) -> (Succ n, succ (n, rec_n)))
          n)

{- See how the Zero and Succ cases return a 'Nat' built from 'Zero' and
   'Succ', building a copy of the 'Nat' that was started with. Compare
   this to the 'thingy' function above.

   It is also possible to avoid the unpacking and repacking of the
   pair in the 'Succ' case by using an "@ pattern" that makes 'x'
   stand for the whole pair, while 'n' and 'n_rec' stand for the first
   and second parts of the pair, respectively:

          (\x@(n,rec_n) -> (Succ n, succ x))

   Note that defining 'recNat' in this way is not necessarily
   recommended, for efficiency reasons. This implementation builds a
   data structure in memory that is an exact copy of the existing
   structure, wasting memory. The point is that 'iterNat' is
   expressive enough to capture this apparently more general recursion
   scheme. This expressivity is important when using functions with
   interfaces like 'iterNat's that don't operate over concrete data
   structures, but operate over the output of some process where there
   is not concrete data structure. -}

{- Now let's look at another example of iteration over data. Here is a
   data type for describing arithmetic expressions consisting of
   numbers and addition: -}

data Expr
  = Number Int
  | Add    Expr Expr
  deriving Show

{- Let's see now how to systematically derive the type of an 'iterExpr'
   function from this data declaration.

   1. We want a function that takes 'Expr's and returns values of any
      type 't', but we don't know yet what the other argument types
      are. So we write this down:

         iterExpr :: ???? -> Expr -> t

   2. Iteration works on a constructor-by-constructor basis --
      replacing each use of a constructor by a function call. So, to
      work out the other argument types, we take the types of the
      constructors:

        λ> :t Number
        Number :: Int -> Expr
        λ> :t Add
        Add :: Expr -> Expr -> Expr

      Plugging these types into the type of iterExpr gives:

        iterExpr :: (Int -> Expr) -> (Expr -> Expr -> Expr) -> Expr -> t
                     \_ Number        \_ Add

   3. Now we systematically replace 'Expr' by 't' in the types taken
      from the constructors. Why? Because we will be using these
      functions to construct new values of type 't', following the
      structure of the 'Expr' that is given to us. If we left them as
      'Expr' then our function would be less general that it could be
      -- we would only be able to construct 'Expr's from 'Expr's.

        iterExpr :: (Int -> t) -> (t -> t -> t) -> Expr -> t

      Performing this change gives us the type of 'iterExpr'.

   EXERCISE: repeat this same process with the Nat type above and with
   the List type ([a]). You should get the same answers as for
   'iterNat' above and 'iterRight' in Ex3.

   Now that we have the type of 'iterExpr', the implementation follows
   the exact same pattern as 'iterNat' above: we match on the
   constructors of 'Expr' and use the corresponding function: -}

iterExpr :: (Int -> t) -> (t -> t -> t) -> Expr -> t
iterExpr number add (Number i) = number i
iterExpr number add (Add d e) =
    add (iterExpr number add d) (iterExpr number add e)

{- In Lecture 10 we will see some more functions that use
   'iterExpr'. -}
