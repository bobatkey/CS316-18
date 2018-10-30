module Lec11Live where

import Prelude hiding (Functor (..), Applicative (..), Monad (..))

{-    LECTURE 11 : APPLICATIVES AND MONADS -}

data Expr
  = Number Int
  | Add    Expr Expr
  | Mul    Expr Expr
  | If0ThenElse Expr Expr Expr
  | Throw
  | Catch  Expr Expr
  | Print  String Expr
  deriving Show

evaluate :: Expr -> PrintAndThrow Int
evaluate (Number i)  = pure i
evaluate (Add e1 e2) = pure (+) <*> evaluate e1 <*> evaluate e2
evaluate (Mul e1 e2) = pure (*) <*> evaluate e1 <*> evaluate e2
evaluate (If0ThenElse c t e) =
  evaluate c `sequ` \n ->
  if n == 0 then evaluate t else evaluate e
{-  
  pure (\n vt ve -> if n == 0 then vt else ve)
  <*> evaluate c
  <*> evaluate t
  <*> evaluate e
-}
evaluate Throw = P [] Nothing
evaluate (Catch eTry eHandle) =
  case evaluate eTry of
    P outputs1 Nothing ->
      case evaluate eHandle of
        P outputs2 r -> P (outputs1 ++ outputs2) r
    r -> r
evaluate (Print s e) =
  case evaluate e of
    P outputs r -> P (s:outputs) r

sequ  :: f a -> (a -> f b) -> f b
sequ = undefined
--(<*>) :: f a -> f (a -> b) -> f b

prog = If0ThenElse (Number 0)
                   (Print "then branch" (Number 1))
                   (Print "else branch" (Number 2))

myThrowingProgram :: Expr
myThrowingProgram =
  (Number 1 `Add` Throw) `Catch` Number 4

myThrowingProgram2 :: Expr
myThrowingProgram2 =
  (Number 1 `Add` Print "about to throw" (Number 1))
  `Catch`
  (Print "catch" (Number 4))

data PrintAndThrow a = P [String] (Maybe a)
  deriving Show

instance Functor PrintAndThrow where
  fmap f (P outputs Nothing)  = P outputs Nothing
  fmap f (P outputs (Just a)) = P outputs (Just (f a))

instance Applicative PrintAndThrow where
  pure x = P [] (Just x)

  P outputs1 Nothing <*> P outputs2 _
    = P (outputs1++outputs2) Nothing
  P outputs1 _       <*> P outputs2 Nothing
    = P (outputs1++outputs2) Nothing
  P outputs1 (Just f) <*> P outputs2 (Just a)
    = P (outputs1 ++ outputs2) (Just (f a))
{-
doOp :: (             a ->               b ->               c) ->
        PrintAndThrow a -> PrintAndThrow b -> PrintAndThrow c
doOp f r1 r2 =
  case r1 of
    (outputs1, Nothing) -> (outputs1, Nothing)
    (outputs1, Just v1) ->
      case r2 of
        (outputs2, Nothing) -> (outputs1 ++ outputs2, Nothing)
        (outputs2, Just v2) -> (outputs1 ++ outputs2, Just (f v1 v2))
-}
class Functor f where
  fmap :: (a -> b) -> f a -> f b

class Functor f => Applicative f where
  pure  :: a -> f a
  (<*>) :: f (a -> b) -> f a -> f b



  -- doOp :: (a -> b -> c) -> f a -> f b -> f c
{-
class Monoid m where
  mempty :: m
  mappend :: m -> m -> m
-}
--ap :: f (a -> b) -> f a -> f b
--ap cF cA = doOp (\f a -> f a) cF cA
