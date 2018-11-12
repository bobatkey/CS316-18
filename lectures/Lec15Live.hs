module Lec15Live where

import Control.Applicative
import Data.Char

{-   LECTURE 15 : YET MORE PARSING -}

newtype Parser a =   -- a parser of things 'a'
   MkParser (String ->    -- a function from strings to
             Maybe ( String -- the possibility of pairs of strings
                   , a))    -- and things

runParser :: Parser a -> String -> Maybe (String, a)
runParser (MkParser p) s = p s

{- The interface:

   - char  :: Parser Char
   - fmap  :: (a -> b) -> Parser a -> Parser b
   - pure  :: a -> Parser a                             "parse nothing"
   - (<*>) :: Parser (a -> b) -> Parser a -> Parser b   "sequence"
   - empty :: Parser a
   - (<|>) :: Parser a -> Parser a -> Parser a          "try the first, then 2nd"
   - (>>=) :: Parser a -> (a -> Parser b) -> Parser b
-}

digit :: Parser Int
digit = do c <- char
           if isDigit c then
             pure (digitToInt c)
           else
             empty
--   char >>= \c -> if isDigit c then pure (digitToInt c) else empty

isChar :: Char -> Parser ()
isChar c = do c' <- char
              if c == c' then pure () else empty

expr :: Parser Expr
expr =  do d <- digit; pure (Digit d)
    <|> parens (do e1 <- expr
                   isChar '+'
                   e2 <- expr
                   pure (Add e1 e2))

parens :: Parser a -> Parser a
parens p = do isChar '('
              x <- p
              isChar ')'
              pure x

data JSON
  = Null
  | Bool   Bool
  | Number Int
  | String String
  | Array  [JSON]
  | Object [(String,JSON)]
  deriving Show

isString :: String -> Parser ()
isString ""     = pure ()
isString (x:xs) = do isChar x; isString xs

pNull :: Parser JSON
pNull = do isString "null"
           pure Null

pBool :: Parser JSON
pBool =  do isString "true"; pure (Bool True)
     <|> do isString "false"; pure (Bool False)

digits :: Parser [Int]
digits =  do d <- digit; ds <- digits; pure (d:ds)
      <|> do d <- digit; pure [d]

ofDigits :: [Int] -> Int
ofDigits xs = go 0 xs
  where go n []     = n
        go n (d:ds) = go (n*10+d) ds

pNumber :: Parser JSON
pNumber = do ds <- digits
             pure (Number (ofDigits ds))

stringChars :: Parser String
stringChars = do c <- char
                 case c of
                   '\\' -> do c <- char
                              cs <- stringChars
                              pure (c:cs)
                   '"'  -> pure []
                   c    -> do cs <- stringChars
                              pure (c:cs)

-- "syuyu\"fyufs"
pString :: Parser JSON
pString = do isChar '"'
             cs <- stringChars
             -- isChar '"'
             pure (String cs)

-- [1,2]

pArray :: Parser a -> Parser [a]
pArray p = do isChar '['
              xs <- sepBy (do isChar ','; whitespace) p
              isChar ']'
              pure xs

pObject :: Parser a -> Parser [(String,a)]
pObject p = do isChar '{'
               fields <- sepBy (isChar ',') (do String name <- pString
                                                isChar ':'
                                                obj <- p
                                                pure (name,obj))
               isChar '}'
               pure fields

pJSON :: Parser JSON
pJSON =  pNull
     <|> pBool
     <|> pNumber
     <|> pString
     <|> fmap (\xs -> Array xs) (pArray pJSON)
     <|> fmap (\xs -> Object xs) (pObject pJSON)

whitespace :: Parser ()
whitespace =  do c <- char; if c == ' ' then whitespace else failure
          <|> pure ()
     
sepBy :: Parser () -> Parser a -> Parser [a]
sepBy sep p = do x <- p
                 ((do sep; xs <- sepBy sep p; pure (x:xs)) <|> pure [x])
          <|> pure []      


{-
  do d1 <- digit
          isChar '+'
          d2 <- digit
          pure (d1, d2)-}

{-   4+((1+2)+3) -}

data Expr
  = Digit Int
  | Add   Expr Expr
  deriving Show






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

instance Functor Parser where
  --fmap :: (a -> b) -> Parser a -> Parser b
  fmap = alter

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


orElse :: Parser a -> Parser a -> Parser a
orElse (MkParser p1) (MkParser p2) =
  MkParser (\s -> case p1 s of
                    Nothing ->
                      p2 s
                    Just (rest,a) ->
                      Just (rest,a))

failure :: Parser a
failure = MkParser (\s -> Nothing)

instance Alternative Parser where
  empty = failure
  (<|>) = orElse

instance Monad Parser where
  return = pure
  (>>=) = sequ

sequ :: Parser a -> (a -> Parser b) -> Parser b
sequ (MkParser p1) f =
  MkParser (\s -> case p1 s of
                    Nothing -> Nothing
                    Just (rest, a) ->
                      -- continue (f a) rest)
                      case f a of
                        MkParser p2 ->
                          p2 rest)
