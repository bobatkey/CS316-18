module Lec16 where

import Data.Foldable

{-   LECTURE 17 : VISITING AND TRAVERSING CONTAINERS

   Or : how do I write a 'foreach' loop in Haskell?

       for (String item : myList) {
           ...
       }


   A problem:

     - We have a container full of values 'box :: c a'

     - We have a checking function 'f :: a -> Maybe b'

     - We want to
         (a) find out if all the 'a's in 'box' are OK
         (b) if so, make a new version of 'box' of type 'c b'
-}

{- A container type: -}

data Tree a
  = Leaf
  | Node (Tree a) a (Tree a)
  deriving Show

{- Visiting pattern 1 : Functor

   A functor applies a function to every element in the container, and
   creates a new container with the same shape, but new elements. -}
instance Functor Tree where
  -- fmap :: (a -> b) -> Tree a -> Tree b
  fmap f Leaf         = Leaf
  fmap f (Node l x r) = Node (fmap f l) (f x) (fmap f r)

-- Not the only Functor!

--checkAll0 :: (Functor c) => c a -> (a -> Maybe b) -> Maybe (c b)
checkAll0 box f = fmap f box






{- Visiting pattern 2 : Foldable

     (Lecture 10)

   A foldable applies a function to every element in the container,
   and combines all the results. -}

{-
class Foldable f where
     fold :: Monoid m => f m -> m
     foldMap :: Monoid m => (a -> m) -> f a -> m
-}

instance Foldable Tree where
  -- fold :: Monoid m => Tree m -> m
  fold Leaf         = mempty
  fold (Node l x r) = fold l `mappend` x `mappend` fold r

  -- f :: (a -> m)
  -- fmap f :: f a -> f m
  -- fold   :: f m -> m
  foldMap f = fold . fmap f

checkAll1 :: (Functor f, Foldable f) =>
             (a -> Maybe b) ->
             f a ->
             Bool
checkAll1 f box = forall (\a -> case f a of Nothing -> False; Just _ -> True) box

fromJust (Just x) = x
fromJust Nothing  = error "unpossible!"

checkAll2 :: (Functor f, Foldable f) =>
             (a -> Maybe b) ->
             f a ->
             Maybe (f b)
checkAll2 f box =
  if checkAll1 f box then Just (fmap fromJust (fmap f box)) else Nothing

append :: [a] -> [a] -> [a]
append []     ys = ys
append (x:xs) ys = x : append xs ys

append' :: [a] -> [a] -> [a]
append' xs ys = if null xs then ys else head xs : append' (tail xs) ys

myContainer = [1,2,3,4,5]
myOtherContainer = [-1,-2,0,1,2]

myCheck i = if i < 3 then Just i else Nothing

newtype All = MkAll Bool

toBool :: All -> Bool
toBool (MkAll b) = b

instance Monoid All where
  mempty                      = MkAll True
  MkAll b1 `mappend` MkAll b2 = MkAll (b1 && b2)

forall :: (Functor f, Foldable f) => (a -> Bool) -> f a -> Bool
forall f c = toBool (foldMap (\a -> MkAll (f a)) c)
-- forall f = toBool . foldMap (MkAll . f)





{- Visiting pattern 3 : Traversable

   A traversable applies a function to every element in the container,
   and (i) creates a new container with the same shape and new
   elements; and (ii) combines results on the side. -}

{-
class Traversable c where
  traverse :: Applicative f => (a -> f b) -> c a -> f (c b)
-}

instance Traversable Tree where
  -- traverse :: Applicative f => (a -> f b) -> Tree a -> f (Tree b)
  traverse f Leaf = pure Leaf
  traverse f (Node l x r) =
    pure Node        -- :: f (Tree a -> a -> Tree a -> Tree a)
    <*> traverse f l -- :: f (Tree a)
    <*> f x          -- :: f a
    <*> traverse f r -- :: f (Tree a)
{-
instance Traversable [] where
  traverse f [] = pure []
  traverse f (x:xs) =
    pure (:) <*> f x <*> traverse f xs
-}

checkAll :: (Traversable c) => (a -> Maybe b) -> c a -> Maybe (c b)
checkAll f box = traverse f box

foreach :: (Traversable c, Applicative f) => c a -> (a -> f ()) -> f ()
foreach box body = pure (\_ -> ()) <*> traverse body box

outputNumbers :: IO ()
outputNumbers =
  foreach [1..]
    (\i -> print i)

checkAll00 :: (a -> Maybe b) -> Tree a -> Maybe (Tree b)
checkAll00 f Leaf = Just Leaf
checkAll00 f (Node l x r) =
  case checkAll00 f l of
    Nothing -> Nothing
    Just l' -> case f x of
                 Nothing -> Nothing
                 Just x' -> case checkAll00 f r of
                              Nothing -> Nothing
                              Just r' -> Just (Node l' x' r')


-- fmap     ::                  (a -> b)   -> c a -> c b
-- foldMap  :: Monoid m =>      (a -> m)   -> c m -> m
-- traverse :: Applicative f => (a -> f b) -> c a -> f (c b)

-- (<*>)    ::                f (a -> b)   -> f a -> f b
-- (>>=)    ::                  (a -> f b) -> f a -> f b
