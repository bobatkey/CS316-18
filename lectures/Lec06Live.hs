module Lec06Live where

import Data.Char
import Prelude hiding (Maybe (..), String, Monoid (..))

{-   LECTURE 06 : DECLARING TYPES AND CLASSES -}

{-   PART I : TYPE SYNONYMS -}

-- Useful for documentation

-- Useful to prevent repeating yourself

-- 1. String

-- typedef int my_int;

type Metres = Int

type DB = [(String,Int,Double)]

type String = [Char]

add :: Metres -> Metres -> Metres
add x y = x + y

-- 2. Positions

type Position = (Int,Int)

origin :: Position
origin = (0,0)

-- 3. Transformations

type Transformation = Position -> Position

goNorth :: Transformation
goNorth (x,y) = (x,y+1)

-- 4. Parameterised abbreviations

type Pair a = (a,a)

type Position' = Pair Int

-- type List a = Maybe (a, List a)





{-   PART II : DATA TYPES -}

-- 1. Enumerations

data Direction
  = North
  | South
  | East
  | West
  deriving Show

-- 2. Maybe (replacement for 'null')
-- https://www.infoq.com/presentations/Null-References-The-Billion-Dollar-Mistake-Tony-Hoare

data Maybe a
  = Nothing
  | Just a
  deriving Show

hd :: [a] -> Maybe a
hd []     = Nothing
hd (x:xs) = Just x

-- 3. "Make Illegal States Unrepresentable"

{-
    public class Student {
       // never null!
       @Nonnull
       private Optional<String> name;

       // at least one of these is non-null
       private String registrationNumber;

       private String dsUsername;

       // ...
    }

    (Optional)
-}

data Student =
  MkStudent { studentName        :: String
            , details            :: StudentDetails
            }
  deriving Show

data StudentDetails
  = OnlyRegNumber String
  | OnlyUsername  String
  | BothRegAndUsername String String
  deriving Show

-- 4. Trees

data List a
  = Nil
  | Cons a (List a)
  deriving Show

data Tree a
  = Leaf
  | Node (Tree a) a (Tree a)
  deriving Show

exampleTree :: Tree Int
exampleTree = Node (Node Leaf 1 Leaf) 2 (Node Leaf 3 Leaf)

-- 5. XML / JSON

data XML
  = Elem { tagName :: String, attributes :: [(String,String)], children :: [XML] }
  | Text String

data JSON
  = Null
  | Boolean Bool
  | String  String
  | Number  Double
  | Object  [(String,JSON)]
  | Array   [JSON]
  deriving (Show)

insert :: (a -> a -> Ordering) -> a -> [a] -> [a]
insert cmp x []     = [x]
insert cmp x (y:ys) = case cmp x y of
                        EQ -> x : y : ys
                        LT -> x : y : ys
                        GT -> y : insert cmp x ys

sort :: (a -> a -> Ordering) -> [a] -> [a]
sort cmp []     = []
sort cmp (x:xs) = insert cmp x (sort cmp xs)

instance Eq JSON where
  Null == Null   = True
  --(==) Null Null = True
  Boolean b1 == Boolean b2 = b1 == b2
  String s1  == String s2  = s1 == s2
  Number d1  == Number d2  = d1 == d2
  Object fields1 == Object fields2 =
       sort (\(nm1,_) (nm2,_) -> compare nm1 nm2) fields1
    == sort (\(nm1,_) (nm2,_) -> compare nm1 nm2) fields2
  Array jsons1 == Array jsons2 = jsons1 == jsons2
  _ == _ = False


upperCase :: JSON -> JSON
upperCase Null            = Null
upperCase (Boolean b)     = Boolean b
upperCase (String s)      = String (map toUpper s)
upperCase (Number d)      = Number d
upperCase (Object fields) = Object (map (\(nm,j) -> (nm, upperCase j)) fields)
upperCase (Array jsons)   = Array (map upperCase jsons)

testJSON :: JSON
testJSON = Object [ ("field1", Number 2.0)
                  , ("field2", String "two point oh")
                  ]

testJSON' :: JSON
testJSON' = Object [ ("field2", String "two point oh")
                  , ("field1", Number 2.0)
                  ]



-- { 'field1': 2.0, 'field2': "TWO POINT OH" }

{-   PART III : TYPE CLASSES -}

-- 1. Show



-- 2. Eq

-- 3. Monoid

class Monoid a where
  mempty :: a
  mappend :: a -> a -> a

instance Monoid Int where
  mempty = 0
  mappend = (+)

instance Monoid [a] where
  mempty = []
  mappend = (++)

instance Monoid Bool where
  mempty = True
  mappend = (&&)

(<>) :: Monoid a => a -> a -> a
(<>) = mappend

crush :: Monoid a => [a] -> a
crush []     = mempty
crush (x:xs) = x <> crush xs


