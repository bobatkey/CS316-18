module Lec14Live where

import Data.Char
import Control.Applicative
import Control.Monad

{-    LECTURE 14 : MORE PARSER COMBINATORS                    -}

newtype Parser a =   -- a parser of things 'a'
   MkParser (String ->    -- a function from strings to
             Maybe ( String -- the possibility of pairs of strings
                   , a))    -- and things

runParser :: Parser a -> String -> Maybe (String, a)
runParser (MkParser p) s = p s

-- "char"

char :: Parser Char
char = MkParser (\s -> case s of
                         (c:rest) -> Just (rest, c)
                         ""       -> Nothing)

-- "alter", and Functors

alter :: (a -> b) -> Parser a     -> Parser b
alter    f           (MkParser p) =
  MkParser (\s -> case p s of
                    Nothing        -> Nothing
                    Just (rest, a) -> Just (rest, f a))

digit :: Parser Int
digit = alter digitToInt char

instance Functor Parser where
  --fmap :: (a -> b) -> Parser a -> Parser b
  fmap = alter

-- "andThen", and Applicatives

-- runParser (andThen digit digit) "12" = Just ("", (1, 2))
-- runParser (andThen digit char)  "12" = Just ("", (1, '2'))
-- runParser (andThen digit char)  "1"  = Nothing

andThen :: Parser a -> Parser b -> Parser (a,b)
andThen (MkParser p1) (MkParser p2) =
  MkParser (\s -> case p1 s of
                    Nothing ->
                      Nothing
                    Just (rest, a) ->
                      case p2 rest of
                        Nothing ->
                          Nothing
                        Just (rest', b) ->
                          Just (rest', (a,b)))

nothing :: a -> Parser a
nothing a = MkParser (\s -> Just (s, a))

instance Applicative Parser where
  -- pure :: a -> Parser a
  pure = nothing
  -- (<*>) :: Parser (a -> b) -> Parser a -> Parser b
  pf <*> pa = fmap (\(f,a) -> f a)  (pf `andThen` pa)

-- fmap :: ((a -> b,a) -> b) -> Parser (a -> b,a) -> Parser b

digitAnd3Char :: Parser (((Int, Char), Char), Char)
digitAnd3Char = digit `andThen` char `andThen` char `andThen` char

digitAnd3Char' :: Parser (Int, Char, Char, Char)
digitAnd3Char' =
  pure (\d c1 c2 c3 -> (d,c1,c2,c3))
  <*> digit <*> char <*> char <*> char

-- postProcess(getMailFromServer(),
--             launchNuclearMissiles(),
--             learn.proper.Object.Programming.hierarchies())

-- "orElse", and Alternatives

orElse :: Parser a -> Parser a -> Parser a
orElse (MkParser p1) (MkParser p2) =
  MkParser (\s -> case p1 s of
                    Nothing ->
                      p2 s
                    Just (rest,a) ->
                      Just (rest,a))

failure :: Parser a
failure = MkParser (\s -> Nothing)

twoChar :: Parser (Char, Char)
twoChar = pure (\c1 c2 -> (c1,c2)) <*> char <*> char

oneChar :: Parser (Char, Char)
oneChar = pure (\c -> (c, 'Q')) <*> char

{-
class Alternative f where
  empty :: f a
  (<|>) :: f a -> f a -> f a
-}

instance Alternative Parser where
  empty = failure
  (<|>) = orElse

-- fmap  ::        (a -> b) -> Parser a -> Parser b
-- (<*>) :: Parser (a -> b) -> Parser a -> Parser b
-- (>>=) :: Parser a -> (a -> Parser b) -> Parser b

-- (<|>) :: Parser a -> Parser a -> Parser a

-- "sequ", and Monads

expectAnA :: Parser ()
expectAnA = undefined -- fmap (\c -> if c == 'A' then () else failure) char

--          Parser a -> (a -> Parser b) -> Parser b

{- class Monad f where
     return :: a -> f a
     (>>=)  :: f a -> (a -> f b) -> f b
-}

instance Monad Parser where
  return = pure
  (>>=) = sequ

sequ :: Parser a -> (a -> Parser b) -> Parser b
sequ (MkParser p1) f =
  MkParser (\s -> case p1 s of
                    Nothing -> Nothing
                    Just (rest, a) ->
                      case f a of
                        MkParser p2 ->
                          p2 rest)

expectAnAB :: Parser ()
expectAnAB = do c <- char
                c' <- char
                if c == 'A' && c' == 'B' then pure ()
                  else failure

{-   char c  = getCharFromInput ();
     char c' = getCharFromInput ();
     if (c == 'A' && c' == 'B') {
         return;
     } else {
         throw new ParseError();
     }
-}


{-
             char >>= (\c ->
             char >>= (\c' ->
             if c == 'A' && c' == 'B' then pure () else failure))
-}




satisfies :: (Char -> Bool) -> Parser Char
satisfies = undefined


-- Higher level combinators: sepBy, etc.


