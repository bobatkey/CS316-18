module Lec13Live where

{- LECTURE 13 : PARSER COMBINATORS -}


{- What is a parser?

     A parser of things is---
       a function
         from strings
           to lists
              of pairs
                 of strings
                    and things!

  -- Fritz Ruehr  (after Dr Suess)
-}

-- (1+2)+3

-- time flies like an arrow
-- fruit flies like a banana

newtype Parser a =   -- a parser of things 'a'
   MkParser (String ->    -- a function from strings
             Maybe ( String -- pairs of strings
                   , a))    -- and things

runParser :: Parser a -> String -> Maybe (String, a)
runParser (MkParser p) s = p s

oldbob :: Parser ()
oldbob = MkParser recogniseBob
  where recogniseBob ('B':'o':'b':rest) = Just (rest, ())
        recogniseBob _                  = Nothing

oldben :: Parser ()
oldben = MkParser recogniseBen
  where recogniseBen ('B':'e':'n':rest) = Just (rest, ())
        recogniseBen _                  = Nothing

-- "(plus Z Z)"

string :: String -> Parser ()
string expected = MkParser (p expected)
  where
    p :: String -> String -> Maybe (String, ())
    p []     rest   = Just (rest, ())
    p (_:_)  []     = Nothing
    p (e:es) (c:cs) | e == c    = p es cs
                    | otherwise = Nothing

bob, ben, fred :: Parser ()
bob = string "Bob"
ben = string "Ben"
fred = string "Fred"

orElse :: Parser a -> Parser a -> Parser a
orElse (MkParser p1) (MkParser p2) =
  -- p1 :: String -> Maybe (String, a)
  -- p2 :: String -> Maybe (String, a)
  MkParser (\s -> case p1 s of
                    Just result -> Just result
                    Nothing     -> p2 s)

failure :: Parser a
failure = MkParser (\s -> Nothing)

data Person
  = Bob
  | Ben
  | Fred
  deriving Show

alter :: (b -> a) -> Parser b -> Parser a
alter f (MkParser p) =
  MkParser (\s -> case p s of
                    Nothing        -> Nothing
                    Just (rest, y) -> Just (rest, f y))

instance Functor Parser where
  fmap f p = alter f p

bob' = alter (\() -> Bob) bob
ben' = alter (\() -> Ben) ben
fred' = alter (\() -> Fred) fred

personToString :: Person -> String
personToString Bob = "Bob"
personToString Ben = "Ben"
personToString Fred = "Fred"

andThen :: Parser a -> Parser b -> Parser (a,b)
andThen (MkParser p1) (MkParser p2) =
  -- p1 :: String -> Maybe (String, a)
  -- p2 :: String -> Maybe (String, b)
  MkParser (\s -> case p1 s of
                    Nothing -> Nothing
                    Just (s0, a) ->
                      case p2 s0 of
                        Nothing -> Nothing
                        Just (s1, b) -> Just (s1, (a,b)))

person :: Parser Person
person = bob' `orElse` ben' `orElse` fred'

sequ parser =
  fmap (\((p,()),ps) -> p:ps) (parser `andThen`
                               string "," `andThen`
                               sequ parser)
  `orElse`
  fmap (\() -> []) (string "")
{-  
  fmap (\((((a,()),b),()),c) -> (a,b,c))
  (          person
     `andThen` string ","
     `andThen` sequ
     `andThen` string ","
     `andThen` person)
-}
