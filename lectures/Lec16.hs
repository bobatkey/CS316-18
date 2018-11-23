module Lec16 where

import Prelude hiding (Traversable (..))
import Data.Foldable

{-   LECTURE 16 : VISITING AND TRAVERSING CONTAINERS

   Or : how do I write a 'foreach' loop in Haskell?

       for (String item : myList) {
           ...
       }

   This lecture is about how to iterate over the elements of a
   container (such as a list), performing some kind of "side
   effecting" operation on each element, and gathering all the results
   into a new container.

   We have already seen a pattern like this with the 'Functor'
   interface from Lecture 09. The Functor type class contains a single
   method:

      class Functor c where
         fmap :: (a -> b) -> c a -> c b

   Instances of 'Functor' include lists, so we have a function:

      fmap :: (a -> b) -> [a] -> [b]

   that applies a function to every element of the list, producing a
   new list with the results.

   Because Haskell is a "pure" language, 'fmap' is quite restricted in
   what it can do. For instance, we can't throw exceptions while
   transforming each element, or do printing, or ask the user for
   input.

   Concretely, let's assume we have the following problem:

     - We have a list of values 'vals :: [a]'

     - We have a function 'f :: a -> Maybe b'

     - We want to:

        (a) find out if all the 'a's in 'vals' are OK (i.e. 'f' does
            not return 'Nothing')

        (b) if so, make a new version of 'vals' of type '[b]'

   For complete concreteness, let's say we have a list of strings,
   such as one of the two following lists: -}

strings1 :: [String]
strings1 = ["1", "2", "3"]

strings2 :: [String]
strings2 = ["1", "two", "3"]

{- And we have a checking function: -}

toDigit :: String -> Maybe Int
toDigit "0" = Just 0
toDigit "1" = Just 1
toDigit "2" = Just 2
toDigit "3" = Just 3
toDigit "4" = Just 4
toDigit "5" = Just 5
toDigit "6" = Just 6
toDigit "7" = Just 7
toDigit "8" = Just 8
toDigit "9" = Just 9
toDigit _   = Nothing

{- Our goal is to write a function, and then generalise a function of
   the following type:

     checkAll :: (a -> Maybe b) -> [a] -> Maybe [b]

   so that:

     checkAll toDigit strings1 == Just [1, 2, 3] :: Maybe [Int]

   and:

     checkAll toDigit strings2 = Nothing :: Maybe [Int]

   We expect 'Just' on 'strings1' because for all of the elements,
   'toDigit' returns 'Just' something. We expect 'Nothing' on
   'strings2' because one of the elements ("two") causes 'toDigit' to
   return 'Nothing'. -}

{-    Part 1 : The 'Direct' Way

   Let's write this function the 'direct' way by using 'case' to do
   pattern matching: -}

checkAll0 :: (a -> Maybe b) -> [a] -> Maybe [b]
checkAll0 f [] = Just []
checkAll0 f (x:xs) =
  case f x of
    Nothing -> Nothing
    Just y ->
      case checkAll0 f xs of
        Nothing -> Nothing
        Just ys -> Just (y:ys)

{- Let's test it:

     > checkAll0 toDigit strings1
     Just [1,2,3]
     > checkAll0 toDigit strings2
     Nothing

   Seems to work!

   However, 'checkAll0' is a little messy -- there's a cascade of
   'case' expressions that does nothing but make sure that if either
   running 'f' or checking the rest of the list return 'Nothing', then
   the whole thing returns 'Nothing'. We've seen a pattern like this
   before in Lectures 10 and 11, and we saw how to use Applicative
   functors to make it tidier. Instead of writing out a cascade of
   'case's, we realise that what we want to do on each element of the
   list is:

     - call 'f' on 'x'
     - call 'checkAll0 f' on 'xs'
     - combine the results with ':' to make a list
     - if any of the above returns 'Nothing', we return 'Nothing'

   We can achieve this by writing the function using the Applicative
   Functor methods 'pure' and '<*>': -}

checkAll1 :: (a -> Maybe b) -> [a] -> Maybe [b]
checkAll1 f []     = pure []
checkAll1 f (x:xs) = pure (:) <*> f x <*> checkAll1 f xs

{- First, let's test it:

     > checkAll1 toDigit strings1
     Just [1,2,3]
     > checkAll1 toDigit strings2
     Nothing

   Seems to work! Why does this work? Let's look at the types.

   In the first case, 'pure' (specialised to 'Maybe') has type 'a ->
   Maybe a', so we can use it to turn an ordinary value '[]' of type
   '[b]' into a 'Maybe [b]'.

   In the second case, we need to remember that '<*>' has the type:

      <*> :: Maybe (a -> b) -> Maybe a -> Maybe b

   '<*>' acts like a kind of enhanced function application: instead of
   just applying a function of type 'a -> b' to a value of type 'a',
   it applies a function wrapped in a 'Maybe' to a value wrapped in a
   'Maybe'. Compare this to the 'bitmapApply' and 'procApply'
   functions from Exercise 3.

   In 'checkAll1', we have:

       pure (:)       :: Maybe (b -> [b] -> [b])
       f x            :: Maybe b
       checkAll1 f xs :: Maybe [b]

   So, using '<*>' we can apply 'pure (:)' to 'f x' and 'checkAll1 f
   xs' to get a value of type 'Maybe [b]'. By using 'pure' and '<*>'
   we have not had to write out a lot of plumbing code to make sure
   'Just's and 'Nothing's go to the right places, we can rely on the
   Maybe instance for Applicative.

   It is worth comparing the code of 'checkAll1' to the code for 'map'
   for lists (renamed to 'map0' here to avoid a name clash): -}

map0 :: (a -> b) -> [a] -> [b]
map0 f [] = []
map0 f (x:xs) = f x : map0 f xs

{- If we remember that the expression 'f x : map0 f xs' is equivalently
   written as:

      (:) (f x) (map0 f xs)

   So the whole function could be written as: -}

map1 :: (a -> b) -> [a] -> [b]
map1 f []     = []
map1 f (x:xs) = (:) (f x) (map0 f xs)

{- Then the similarity with 'checkAll1' becomes a matter of replacing
   the 'pure' and '<*>' in 'checkAll1' with whitespace -- function
   application in Haskell is so common that it is written with no
   syntax at all, but if we want function applicatio with side effects
   then we need to insert 'pure' and '<*>' in the right places.

   Comparing 'map1' and 'checkAll1', we can see that 'checkAll1' is
   really a kind of 'map' but with side effects. 'checkAll1' is
   restricted to the side effect being "might throw an error", but
   this restriction is artificial. -}


{-     Part 2 : Generalising to other side effects

   Looking at 'checkAll1', we can see that not only does it not
   mention 'Nothing' and 'Just' explicitly, it isn't specific to
   'Maybe' at all. Indeed, if we comment out the type declaration for
   'checkAll1' and ask GHCi what type it ought to have, then we get a
   more general type than the one written above:

        > :t checkAll1
        checkAll1 :: Applicative f => (t1 -> f t) -> [t1] -> f [t]

   Let's write a new version of 'checkAll1' that uses this more
   general type signature (with nicer choices for the type names), but
   keeps exactly the same code: -}

checkAll :: Applicative f => (a -> f b) -> [a] -> f [b]
checkAll f []     = pure []
checkAll f (x:xs) = pure (:) <*> f x <*> checkAll f xs

{- This new version now works for any Applicative Functor, and so many
   kinds of possible 'side effect'. For example, we can write a
   function that does I/O to ask the user whether to allow a number or
   to negate it: -}

askUser :: Int -> IO Int
askUser i =
  do putStrLn ("What about: " ++ show i ++ "?")
     response <- getLine
     case response of
       "negate" -> pure (-i)
       _        -> pure i

{- Now running 'checkAll' with 'askUser' asks the user for their
   opinion on each element of the input list:

         > checkAll askUser [1,2,3]
         What about: 1?
         negate
         What about: 2?
         ok
         What about: 3?
         negate
         [-1,2,-3]

   So, checkAll is now a general function for 'mapping over' a list
   whilst performing some side effects. Since the side effects may be
   arbitrary and not just 'Maybe', the name 'checkAll' is now
   inappropriate. Following Haskell tradition, we should call this
   function 'traverse', but we won't due to name clashes with the
   standard library: -}

traverse0 :: Applicative f => (a -> f b) -> [a] -> f [b]
traverse0 f []     = pure []
traverse0 f (x:xs) = pure (:) <*> f x <*> traverse0 f xs

{- (This has exactly the same code as 'checkAll1' and 'checkAll', but
   now the name is more sensible.) -}

{- Now that we have identified 'traverse0' as a sensible function, we
   can think of other specialised versions of it. For example, if we
   swap the order of the arguments, then we get a function that we can
   think of as a kind of Java-style 'for (Item i : container) {...}'
   loop (again a '0' is appended to the name to avoid a name clash): -}

for0 :: Applicative f => [a] -> (a -> f b) -> f [b]
for0 xs f = traverse0 f xs

{- For example:

         > for0 [1,2,3] askUser
         What about: 1?
         negate
         What about: 2?
         nothing
         What about: 3?
         nothing
         [-1,2,3]

   Or, more explicitly:

         > for0 [1,2,3] (\i -> askUser i)
         What about: 1?
         nothing
         What about: 2?
         negate
         What about: 3?
         meh
         [1,-2,3]
-}



{-    Part 3 : Generalising to other containers

   Just as 'map' can be generalised from lists to lots of other kinds
   of container, 'traverse' can be as well.

   For example, here are 'Tree's again, and 'map' for trees, as we saw
   in Lecture 09 and Exercise 3: -}

data Tree a
  = Leaf
  | Node (Tree a) a (Tree a)
  deriving Show

mapTree :: (a -> b) -> Tree a -> Tree b
mapTree f Leaf         = Leaf
mapTree f (Node l x r) = Node (mapTree f l) (f x) (mapTree f r)

{- And here is the corresponding 'traverseTree', which is similar to
   'mapTree', except for the addition of 'pure' and '<*>' at the right
   places. -}

traverseTree :: Applicative f => (a -> f b) -> Tree a -> f (Tree b)
traverseTree f Leaf         = pure Leaf
traverseTree f (Node l x r) = pure Node <*> traverseTree f l <*> f x <*> traverseTree f r

{- We can go on to define 'traverse' like functions for almost all
   containers, such as Maybe and the standard library's Data.Map.Map
   type.

      EXERCISE: define
        'traverseMaybe :: Applicative f => (a -> f a) -> Maybe a -> f (Maybe b)'

   Now we can spot a recurring pattern -- many containers have a
   'traverse'-like function. Just as we did for 'mapList', 'mapTree',
   'mapMaybe' etc., we can gather all these examples up into a single
   type class that will allow us to write functions that are generic
   in the actual container being used. We call this type class
   'Traversable': -}

class Traversable c where
  traverse :: Applicative f => (a -> f b) -> c a -> f (c b)

{- (This type class is defined in the standard library, but I have
    hidden the standard definition in the 'import' declarations above
    so that I can repeat it here. The standard one differs in that it
    requires all Traversables to also be Foldable and Functor (Lecture
    09), but I'll ignore that here.)

   Now list and 'Tree's (and 'Maybe') are all instances of
   'Traversable': -}

instance Traversable [] where
  traverse f []     = pure []
  traverse f (x:xs) = pure (:) <*> f x <*> traverse f xs

instance Traversable Tree where
  traverse f Leaf         = pure Leaf
  traverse f (Node l x r) = pure Node <*> traverse f l <*> f x <*> traverse f r

{-   EXERCISE: write an instance of Traversable for Maybe -}

{-   EXERCISE: can you write 'fmap' if you are given 'traverse'? -}



{- Once we have identified 'Traversable' as a type class, we can write
   some generic functions that use 'traverse' in various ways. For
   example, the 'for0' function above works for all 'Traversable's: -}

for :: (Traversable c, Applicative f) => c a -> (a -> f b) -> f (c b)
for c f = traverse f c

{- Now we can write a generic function that traverses over any
   Traversable container of integers, and prints out all the items
   contained within: -}

outputNumbers :: Traversable c => c Int -> IO (c Int)
outputNumbers c =
  for c (\i -> do print i; return i)

{- For example:

      > outputNumbers [1,2,3]
      1
      2
      3
      [1,2,3]
      > outputNumbers (Node Leaf 1 (Node Leaf 2 Leaf))
      1
      2
      Node Leaf 1 (Node Leaf 2 Leaf)
-}

{- Another useful function is 'sequence' (called 'sequenceA' for
   historical reasons), which takes a container of tasks to a task of
   containers (thinking of a value of type 'f a' where 'f' is an
   Applicative Functor as a "task" that produces a value of type 'a'): -}

sequence :: (Applicative f, Traversable c) => c (f a) -> f (c a)
sequence c = traverse id c

myTasks = [ putStrLn "Task 1"
          , do { line <- getLine; putStrLn ("You said " ++ line) }
          , do { putStrLn "Task 3" }
          ]
