module Tut08Live where

import Control.Applicative

{----------------------------------------------------------------------}
{- GENERATING COMBINATIONS                                            -}
{----------------------------------------------------------------------}

type Coin = Int

ukCoins :: [Coin]
ukCoins = [50,20,10,5,2,1]

usCoins :: [Coin]
usCoins = [25,10,5,2,1]

makeChange :: Int -> [Coin] -> [Coin] -> [[Coin]]
makeChange 0      selection coins     = [selection]
makeChange amount selection []        = empty
makeChange amount selection (c:coins) =
  if c > amount then
    makeChange amount selection coins
  else
    makeChange (amount - c) (c:selection) (c:coins)
    <|>
    makeChange amount selection coins

-- while (amount > 0) {
--   int c = getNextCoin();
--   if (c > amount) {
--      discardCoin();
--   } else {
--      
-- }




{----------------------------------------------------------------------}
{- STATE MONAD                                                        -}
{----------------------------------------------------------------------}

newtype State s a = St (s -> (s,a))

-- instance Functor (c -> )
--   fmap :: (a -> b) -> (c -> a) -> (c -> b)

-- instance Functor (c,) where
--   fmap :: (a -> b) -> (c,a) -> (c,b)

instance Functor (State s) where
  fmap f (St t) = St (fmap (fmap f) t)

instance Applicative (State s) where
  pure x = St (\s -> (s,x))
  St t1 <*> St t2 = St (\s -> case t1 s of
                                (s',f) -> case t2 s' of
                                            (s'',a) -> (s'',f a))

instance Monad (State s) where
  return = pure
  -- (>>=) :: State s a -> (a -> State s b) -> State s b
  St t >>= f = St (\s -> case t s of
                           (s',a) -> case f a of
                                       St t2 ->
                                         t2 s')

get :: State s s
get = St (\s -> (s,s))

put :: s -> State s ()
put new_state = St (\_ -> (new_state,()))

runState :: State s a -> s -> a
runState (St t) s = case t s of
                      (_, a) -> a

program :: State Int Int
program = do put 5
             x <- get
             put (x+4)
             get



newtype NDState s a = ND (s -> [(s,a)])

instance Functor (NDState s) where
  fmap f (ND t) = ND (fmap (fmap (fmap f)) t)

instance Applicative (NDState s) where
  pure x = ND (\s -> [(s,x)])
  ND t1 <*> ND t2 = ND (\s -> [ (s'',f a) | (s',f) <- t1 s, (s'',a) <- t2 s' ])

instance Monad (NDState s) where
  return = pure
  -- (>>=) :: NDState s a -> (a -> NDState s b) -> NDState s b
  ND t >>= f = ND (\s -> [ (s'',b)  | (s',a) <- t s
                                    , let ND t2 = f a
                                    , (s'',b) <- t2 s'])

instance Alternative (NDState s) where
  empty = ND (\_ -> [])
  ND t1 <|> ND t2 = ND (\s -> t1 s ++ t2 s)

get' :: NDState s s
get' = ND (\s -> [(s,s)])

put' :: s -> NDState s ()
put' new_state = ND (\_ -> [(new_state,())])

runNDState :: NDState s a -> s -> [a]
runNDState (ND t) s = [ a | (_,a) <- t s ]


program' :: NDState Int Int
program' = do put' 5
              x <- get'
              (put' (x+4) <|> put' (x+3))
              get'


nextCoin :: NDState [Coin] Coin
nextCoin = do coins <- get'
              case coins of
                []    -> empty
                (c:_) -> pure c

discardCoin :: NDState [Coin] ()
discardCoin = do coins <- get'
                 case coins of
                   []         -> empty
                   (_:coins') -> put' coins'

guard True = pure ()
guard False = empty

makeChange2 :: Int -> [Coin] -> NDState [Coin] [Coin]
makeChange2 0      selection = pure selection
makeChange2 amount selection =
  do c <- nextCoin
     if c > amount then
       do discardCoin
          makeChange2 amount selection
     else
       (do discardCoin
           makeChange2 amount selection)
        <|>
       (do makeChange2 (amount - c) (c:selection))

makeChange3 :: Int -> [Coin] -> NDState [Coin] [Coin]
makeChange3 0      selection = pure selection
makeChange3 amount selection =
  (do c <- nextCoin
      guard (c <= amount)
      makeChange3 (amount - c) (c:selection))
  <|>
  (do discardCoin
      makeChange3 amount selection)
















{-
makeChange :: Int -> [Coin] -> [Coin] -> [[Coin]]
makeChange 0      coins     selection = [selection]
makeChange amount []        selection = []
makeChange amount (c:coins) selection
  | c > amount = makeChange amount coins selection
  | otherwise  =
       makeChange amount       coins selection
    ++ makeChange (amount - c) coins (c:selection)
-}
