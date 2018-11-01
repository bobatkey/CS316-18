module Tut05Live where

-- 1. iterList

mapList :: (a -> b) -> [a] -> [b]
mapList f []     = []
mapList f (x:xs) = f x : mapList f xs

mulList :: [Int] -> Int
mulList []     = 1
mulList (x:xs) = x * mulList xs

iterList :: b -> (a -> b -> b) -> [a] -> b
iterList z f []     = z
iterList z f (x:xs) = f x (iterList z f xs)

mapList' :: (a -> b) -> [a] -> [b]
mapList' f = iterList [] (\x xs -> f x:xs)

-- 2. Parser Combinators

type Parser a = String -> Maybe (String,a)

openCurly :: Parser ()
openCurly ('{':rest) = Just (rest, ())
openCurly _          = Nothing

num :: Parser Int
num ('0':rest) = Just (rest,0)
num ('1':rest) = Just (rest,1)
num ('2':rest) = Just (rest,2)
num ('3':rest) = Just (rest,3)
num ('4':rest) = Just (rest,4)
num ('5':rest) = Just (rest,5)
num ('6':rest) = Just (rest,6)
num ('7':rest) = Just (rest,7)
num ('8':rest) = Just (rest,8)
num ('9':rest) = Just (rest,9)
num _          = Nothing

-- S ::= '{' | '0' | '1' | ... | '9'

orElse :: Parser a -> Parser a -> Parser a
orElse p1 p2 inp =
  case p1 inp of
    Nothing -> p2 inp
    Just a  -> Just a

mapParser :: (a -> b) -> Parser a -> Parser b
mapParser f p inp = case p inp of
                      Nothing       -> Nothing
                      Just (rest,a) -> Just (rest, f a)

andThen :: Parser a -> Parser b -> Parser (a,b)
andThen p1 p2 inp =
  case p1 inp of
    Nothing -> Nothing
    Just (rest,a) ->
      case p2 rest of
        Nothing -> Nothing
        Just (rest',b) -> Just (rest',(a,b))

eoi :: Parser ()
eoi "" = Just ("", ())
eoi _  = Nothing

-- N ::= 'n' N | $

number :: Parser [Int]
number = (mapParser (\(n,ns) -> n:ns) (num `andThen` number))
         `orElse`
         (mapParser (\() -> [])       eoi)

properNumber :: Parser Int
properNumber = mapParser listToProperNumber number

listToProperNumber :: [Int] -> Int
listToProperNumber = iterList 0 (\d n -> 10*n + d) . reverse

-- YACC
-- Yet Another Compiler Compiler


-- number:
--   | DIGIT number       { make_list($1,$2) }
--   | EOI                { make_nil() }
