## Tutorial 2 Unassessed Homework

 1. [H3.3] What are the types of the following functions?

     ```haskell
     second xs = head (tail xs)

     swap (x, y) = (y, x)

     pair x y = (x, y)

     double x = x * 2

     palindrome xs = reverse xs == xs

     twice f x = f (f x)
     ```

     Hint: Take care to include the necessary class constraints in the
     types if the functions are defined using overloaded operators.

 2. [H4.8] The Luhn algorithm is used to check bank card numbers for
    simple errors such as mistyping a digit, and proceeds as follows:

      * consider each digit as a separate number;
      * moving left, double every other number from the second last;
      * subtract 9 from each number that is now greater than 9;
      * add all the resulting numbers together;
      * if the total is divisible by 10, the card number is valid.

      1.  Define a function `luhnDouble :: Int -> Int` that doubles a digit
          and subtracts 9 if the result is greater than 9. For example

                  ```shell
                  > luhnDouble 3
                  6

                  > luhnDouble 6
                  3
                  ```

      2. Using `luhnDouble` and the integer remainder function `mod`, define a function `luhnFour :: Int -> Int -> Int -> Int -> Bool` that decides if a four-digit bank card number is valid: For example:

                  ```shell
                  > luhnFour 1 7 8 4
                  True

                  > luhnFour 4 7 8 3
                  False
                  ```

      3. Using `luhnDouble` and `mod` again, define a more general version `luhn :: [Int] -> Bool` of `luhnFour` that accepts card numbers of any length.

 3. [H5.4] In a similar way to the function length, show how the library function `replicate :: Int -> a -> [a]` that produces a list of identical elements can be defined using a list comprehension. For example:

           ```shell
           > replicate 3 True
           [True,True,True]
           ```

 4. Recursion with accumulators.

     1. Consider the following straightforward definition of a function that reverses a list:

               ```haskell
               rev :: [a] -> [a]
               rev []       = []
               rev (x : xs) = rev xs ++ [x]
               ```

        Because of the use of `(++)`, this will have O(n^2) run-time complexity. Rewrite the function using an *accumulator* to improve its efficiency: define a function `revAcc :: [a] -> [a] -> [a]` such that `rev xs = revAcc xs []`. Hint: you shouldn't use `(++)`, but instead "accumulate" the "partial result" of the computation in the extra argument, and return that at the end.

     2. Accumulator-style programming can also be used to turn recursive functions into so-called *tail-recursive* functions, that is, functions where all recursive calls are final calls (calls where no further computation needs to happen afterwards). For instance, the following (silly) function is tail-recursive:

                ```haskell
                foo :: Bool -> Int -> Bool
                foo True n = n
                foo False m = foo True (1+m)
                ```

        while this (equally silly) one is not:

                ```haskell
                goo :: Bool -> Int -> Bool
                goo True n = n
                goo False m = 1 + goo True m
                ```

        Why not? Because the recursive call `goo True m` is not final: after computing it, we still need to compute `1 + ` the value of it. It is easier for compilers to optimise tail-recursive functions, because there is no need to save the current environment to return to after the call.

        Use an accumulator to turn the naive function

                ```haskell
                fac :: Integer -> Integer
                fac 0 = 1
                fac n = n * fac (n - 1)
                ```

        into a tail-recursive function, i.e. define `facAcc :: Integer -> Integer -> Integer` and `fac' n = facAcc n initial` for a suitably chosen `initial :: Integer`. (Laziness makes it harder to reason about efficiency. Without jumping through further hoops, you might not notice a difference in practice between `fac` and `fac'`.)
