module Tut02 where

second :: [a] -> a
second xs = head (tail xs)

head' :: [a] -> Maybe a
head' []     = Nothing
head' (x:xs) = Just x

swap :: (a, a) -> (a, a)
swap (x, y) = (y, x)

pair :: a -> b -> (a, b)
pair x y = (x, y)

double :: Num a => a -> a
double x = x * 2

palindrome :: Eq c => [c] -> Bool
palindrome xs = reverse xs == xs

twice :: (e -> e) -> e -> e
twice f x = f (f x)

----------------------------------------------------------------------

replicate' :: Int -> a -> [a]
replicate' n x = [ x | _ <- [1..n] ]
  -- range(1,10)

----------------------------------------------------------------------

-- accumulators

rev :: [a] -> [a]
rev [] = []
rev (x:xs) = rev xs ++ [x]

--   rev [3,2,1]
-- = rev [2,1] ++ [3]
-- = (rev [1] ++ [2]) ++ [3]
-- = ((rev [] ++ [1]) ++ [2]) ++ [3]
-- = (([] ++ [1]) ++ [2]) ++ [3]

-- revAcc xs ys == rev xs ++ ys
revAcc :: [a] -> [a] -> [a]
revAcc []     ys = ys
revAcc (x:xs) ys -- = rev (x:xs) ++ ys
                 -- = (rev xs ++ [x]) ++ ys
                 -- = rev xs ++ ([x] ++ ys)
                 -- = rev xs ++ (x:ys)
                    = revAcc xs (x:ys)

--   revAcc [3,2,1] []
-- = revAcc [2,1] [3]
-- = revAcc [1] [2,3]
-- = revAcc [] [1,2,3]
-- = [1,2,3]

fac :: Integer -> Integer
fac 0 = 1
fac n = n * fac (n-1)

fac' :: Integer -> Integer -> Integer
fac' 0 acc = acc
fac' n acc = fac' (n-1) $! (n * acc)

--   fac' 4 1
-- = fac' 3 (4 * 1)
-- = fac' 2 (3 * (4 * 1))
-- = fac' 1 (2 * (3 * (4 * 1)))
-- = fac' 0 (1 * (2 * (3 * (4 * 1))))
-- = (1 * (2 * (3 * (4 * 1))))

fac'' :: Integer -> Integer
fac'' n = fac' n 1

ignore :: a -> Int
ignore x = 10
