module Lec12Live where

{-    LECTURE 12 : MONADS                       -}

{- 1. Recap some abstractions -}

f :: Int -> IO Int
f x = do print "running f (FIXME: remove this code before submitting)"; return (x+1)

class MyFunctor f where
  myfmap :: (a -> b) -> f a -> f b

class MyFunctor f => MyApplicative f where
  mypure  :: a -> f a
  (<**>) :: f (a -> b) -> f a -> f b

data Printing a = MkP [String] a
  deriving Show

instance Functor Printing where
  fmap f (MkP outputs a) = MkP outputs (f a)

instance Applicative Printing where
  -- pure :: a -> Printing a
  pure x = MkP [] x

  -- (<*>) :: Printing (a->b) -> Printing a -> Printing b
  MkP outputs1 f <*> MkP outputs2 a =
    MkP (outputs1 ++ outputs2) (f a)

pr :: String -> Printing ()
pr s = MkP [s] ()

computation :: Int -> Printing Int
computation x = pure (\() -> x+1) <*> pr "called 'computation'"
{-
      pr "called 'computation'" :: Printing ()
      (\() -> x+1)              :: () -> Int
      pure (\() -> x+1)         :: Printing (() -> Int)
      pure (\() -> x+1) <*> pr "called 'computation'" :: Printing Int
-}

-- sequ :: Process a -> (a -> Process b) -> Process b
-- sequ = undefined
{-
class Applicative f => Monad f where
  return :: a -> f a 
  (>>=)  :: f a -> (a -> f b) -> f b
-}
instance Monad Printing where
  return = pure

  -- (>>=) :: Printing a -> (a -> Printing b) -> Printing b
  MkP outputs1 a >>= f = MkP (outputs1++outputs2) b
    where
      MkP outputs2 b = f a
    
computation' :: Int -> Printing Int
computation' x =
  do pr "called 'computation'"
     pr "printing again"
     return (x+1)

-- do c1        ===>     c1 >>= \() -> c2
--    c2

-- do x <- c1   ===>     c1 >>= \x -> c2
--    c2 


{- 2. The 'Maybe' monad -}

{- data Maybe a = Nothing
                | Just a
-}
{-
instance Monad Maybe where
  return x = Just x
  -- (>>=) :: Maybe a -> (a -> Maybe b) -> Maybe b
  Nothing >>= f = Nothing
  Just a  >== f = f a
-}

abort :: Maybe a
abort = Nothing

computation3 :: Int -> Maybe Int
computation3 x =
  if x > 5 then abort else return (x+1)

data Tree a = Leaf | Node (Tree a) a (Tree a) deriving Show

checkTree :: Tree Int -> Maybe ()
checkTree Leaf = return ()
checkTree (Node l a r) =
  if a < 0 then abort
  else do checkTree l
          checkTree r

{- 3. The 'List' monad -}
{-
instance Monad [] where
  return x = [x]
  -- (>>=) :: [a] -> (a -> [b]) -> [b]
  as >>= f = concat (map f as)
-}

failure' :: [a]
failure' = []

triples :: [(Int,Int,Int)]
triples = do
  x <- [1..20]
  y <- [1..20]
  z <- [1..20]
  if x*x + y*y == z*z then
    return (x,y,z)
  else
    failure'

{- 5. The 'IO' monad -}

{-
class Applicative f => Monad f where
  return :: a -> f a 
  (>>=)  :: f a -> (a -> f b) -> f b
-}

computation4 :: IO ()
computation4 =
  do putStrLn "Hello, what is your name?"
     name <- getLine
     putStrLn ("Hello " ++ name)
