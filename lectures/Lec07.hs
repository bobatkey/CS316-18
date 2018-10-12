module Lec07 where

import Test.QuickCheck
import Data.Foldable

{-   LECTURE 07 : QUICKCHECK -}


-- 1. Individual testing

list_append_test1 :: Bool
list_append_test1 = [1,2,3] ++ [] == [1,2,3]

list_append_test2 :: Bool
list_append_test2 = [1,2,3,4] ++ [] == [1,2,3,4]

list_append_tests :: [Bool]
list_append_tests = [ list_append_test1
                    , list_append_test2
                    ]

-- 2. Property based testing

list_append_prop1 :: [Int] -> Bool
list_append_prop1 xs = xs ++ [] == xs

list_append_prop2 :: [Int] -> Bool
list_append_prop2 xs = [] ++ xs == xs

list_append_prop3 :: [Int] -> [Int] -> [Int] -> Bool
list_append_prop3 xs ys zs = (xs ++ ys) ++ zs == xs ++ (ys ++ zs)

-- Monoids

monoid_prop1 :: (Eq m, Monoid m) => m -> Bool
monoid_prop1 x = x `mappend` mempty == x

monoid_prop2 :: (Eq m, Monoid m) => m -> Bool
monoid_prop2 x = mempty `mappend` x == x

monoid_prop3 :: (Eq m, Monoid m) => m -> m -> m -> Bool
monoid_prop3 x y z = (x `mappend` y) `mappend` z == x `mappend` (y `mappend` z)
{-
data RGBA = MkRGBA { redChannel   :: Double
                   , greenChannel :: Double
                   , blueChannel  :: Double
                   , alphaChannel :: Double
                   }
  deriving (Show, Eq)

instance Arbitrary RGBA where
  arbitrary = MkRGBA <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance Monoid RGBA where
  mempty =
    MkRGBA 0 0 0 0

  mappend (MkRGBA r1 g1 b1 0) (MkRGBA r2 g2 b2 0) = mempty
  mappend (MkRGBA r1 g1 b1 a1) (MkRGBA r2 g2 b2 a2) = MkRGBA r g b a
    where
      a = a1 + a2 - a1*a2
      r = (a1*r1 + (1-a1)*a2*r2) / a
      g = (a1*g1 + (1-a1)*a2*g2) / a
      b = (a1*b1 + (1-a1)*a2*b2) / a


colour_prop1 = monoid_prop1 :: RGBA -> Bool
-}

data Trit = True3 | False3 | Unknown deriving (Eq, Show)

tritAnd :: Trit -> Trit -> Trit
tritAnd False3 _ = False3
tritAnd _ False3 = False3
tritAnd Unknown _ = Unknown
tritAnd _ Unknown = Unknown
tritAnd True3 True3 = True3

instance Arbitrary Trit where
  arbitrary = oneof [ pure True3, pure False3, pure Unknown ]

instance Monoid Trit where
  mempty = True3
  mappend = tritAnd


-- 3. Reference implementation testing


insert :: Ord a => a -> [a] -> [a]
insert x [] = [x]
insert x (y:ys)
  | x <= y    = x : y : ys
  | otherwise = y : insert x ys

isort :: Ord a => [a] -> [a]
isort [] = []
isort (x:xs) = insert x (isort xs)

isSorted :: Ord a => [a] -> Bool
isSorted []       = True
isSorted [x]      = True
isSorted (x:y:ys) = x <= y && isSorted (y:ys)

insert_preserves_sortedness :: Double -> [Double] -> Bool
insert_preserves_sortedness x xs =
  isSorted (insert x (makeSorted 0 xs))

makeSorted :: Double -> [Double] -> [Double]
makeSorted i [] = []
makeSorted i (x:xs) = y : makeSorted y xs
  where y = i + abs x


{-
module Lec08 where

import Test.QuickCheck

{-   LECTURE 08: QUICKCHECK -}

{-   PART I : WRITING INDIVIDUAL TEST CASES -}

-- artisanal testing, one at a time

append_test_1 :: Bool
append_test_1 =
  [1,2,3] ++ [4,5,6] == [1,2,3,4,5,6]

append_test_2 :: Bool
append_test_2 =
  [4,5,6] ++ [1,2,3] == [4,5,6,1,2,3]

append_test_3 :: Bool
append_test_3 =
  [] ++ [1,2,3] == [1,2,3]

append_test_4 :: Bool
append_test_4 =
  [1,2,3] ++ [] == [1,2,3]

append_tests :: Bool
append_tests =
  and [ append_test_1
      , append_test_2
      , append_test_3
      , append_test_4
      ]

insert :: Ord a => a -> [a] -> [a]
insert x [] = [x]
insert x (y:ys)
  | x <= y    = x : y : ys
  | otherwise = y : insert x ys

insert_test_1 :: Bool
insert_test_1 =
  insert 3 [1,2,4,5] == [1,2,3,4,5]



{-   PART II : PROPERTY BASED TESTING WITH QUICKCHECK -}

-- http://www.cs.tufts.edu/~nr/cs257/archive/john-hughes/quick.pdf

-- Why not test with lots of examples, not just one?

append_left_nil_prop :: [Int] -> Bool
append_left_nil_prop xs =
  [] ++ xs == xs

append_right_nil_prop :: [Int] -> Bool
append_right_nil_prop xs =
  xs ++ [] == xs

append_faulty_prop :: [Int] -> Bool
append_faulty_prop xs =
  xs ++ [0] == xs

-- (x + y) + z = x + (y + z)

append_assoc :: [Int] -> [Int] -> [Int] -> Bool
append_assoc xs ys zs =
  (xs ++ ys) ++ zs == xs ++ (ys ++ zs)

reverse_reverse_prop :: [Int] -> Bool
reverse_reverse_prop xs =
  reverse (reverse xs) == xs

reverse_does_nothing :: [Int] -> Bool
reverse_does_nothing xs =
  reverse xs == xs

reverse_append :: [Int] -> [Int] -> Bool
reverse_append xs ys =
  reverse (xs ++ ys) == reverse ys ++ reverse xs

slow_reverse :: [a] -> [a]
slow_reverse [] = []
slow_reverse (x:xs) = slow_reverse xs ++ [x]

reverse_eq_slow_reverse :: [Int] -> Bool
reverse_eq_slow_reverse xs =
  reverse xs == slow_reverse xs

----------------------------------------------------------------------
isSorted :: Ord a => [a] -> Bool
isSorted []       = True
isSorted [x]      = True
isSorted (x:y:ys) = x <= y && isSorted (y:ys)

insert_preserves_sortedness :: Int -> [Int] -> Bool
insert_preserves_sortedness x xs =
  isSorted (insert x (makeSorted 0 xs))

makeSorted :: Int -> [Int] -> [Int]
makeSorted i [] = []
makeSorted i (x:xs) = y : makeSorted y xs
  where y = i + abs x

makeSorted_prop :: [Int] -> Bool
makeSorted_prop xs =
  isSorted (makeSorted 0 xs)

  
----------------------------------------------------------------------
data Tree a
  = TLeaf
  | TNode (Tree a) a (Tree a)
  deriving (Show, Eq)

instance Arbitrary a => Arbitrary (Tree a) where
  arbitrary = genTree 3

genTree :: Arbitrary a => Int -> Gen (Tree a)
genTree 0 = return TLeaf
genTree n = frequency [ (3, do l <- genTree (n-1)
                               x <- arbitrary
                               r <- genTree (n-1)
                               return (TNode l x r))
                      , (1, return TLeaf)
                      ]


-}
