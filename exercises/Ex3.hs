{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-} 
module Ex3 where

import Control.Exception       (finally)
import Data.ByteString.Builder (Builder, word32LE, word8, word16LE, hPutBuilder)
import Data.Foldable           (fold)
import Data.Semigroup          (Semigroup ((<>)))
import Data.Word               (Word8, Word32)
import System.IO               (openFile, IOMode (..), hClose)
import Text.Read               (readMaybe)

{----------------------------------------------------------------------}
{- CS316 (2018/19) EXERCISE 3                                         -}
{----------------------------------------------------------------------}

-- Submit by committing to GitLab at or before 2pm on Monday 5th
-- November.  There will be a test on this exercise in the lab on that
-- date.
--
-- Your combined score from the submission and the test will be worth
-- 30% of the overall marks for the class (so one mark, below is worth
-- half a percent).
--
-- The test will consist of another file which will import this
-- file. You will need to answer the questions in that file, and
-- commit both by the end of the lab session.

{----------------------------------------------------------------------}
{- HIGHER ORDER PROGRAMMING                                           -}
{----------------------------------------------------------------------}

{- This exercise is focused on programming with "Higher order"
   functions, as introduced in Lecture 05.  A higher order function is
   a function that takes as input other functions. The name 'higher
   order' comes from the following classification of entities that one
   might find in a programming language:

   - 0th order entities are "data" (basically anything that can be printed out)
   - 1st order entities are functions that take "data" to "data"
   - 2nd order entities are functions that take (functions that take "data" to "data") to "data"
   - 3rd order entities are functions that take 2nd order entities to "data"
   - .. and so on

   In general, it is rare to see anything above 3rd order, but it can occur. 

   In other languages, functions that take other functions as input
   are often said to take a 'callback' function. Examples include
   methods that perform work asynchronously and call the given
   function when the work is done. Another example is the
   'java.util.List.sort' method:

      https://docs.oracle.com/javase/9/docs/api/java/util/List.html#sort-java.util.Comparator-

   which takes a 'Comparator' argument, which is (in Java terminology)
   a 'Functional Interface', which is essentially a function. We'll
   see an example of a sorting function that takes a compare as an
   argument later on. -}

{----------------------------------------------------------------------}
{- 1. STRUCTURAL RECURSION ON TREES AND LISTS                         -}
{----------------------------------------------------------------------}

{- In the last exercise, we asked you to write functions by recursion on
   lists and trees. These functions only did one thing. For example,
   'concLists' concatenated lists.

   In this exercise, you will be using and writing so-called "higher
   order" functions that can do many different things, depending on
   the functions that are passed to them as parameters. -}

{- 3.1.0 Discarding. Write filter's complement that retains the elements
   of a list that fail the test rather than those that pass. Write
   your function using 'filter'. -}

discard :: (a -> Bool) -> [a] -> [a]
discard = undefined

{- 1 MARK -}

{- Recursion on lists. Many of the functions we have written on lists
   follow a similar pattern: they do something to the empty list, and
   for (x:xs) they take the value 'x', the result of processing 'xs'
   and combine them in some way. We will look at this general
   "recursion scheme" idea in Lecture 08. Here is a general recursive
   function for lists: -}

iterList :: (a -> b -> b) -> b -> [a] -> b
iterList f z []     = z
iterList f z (x:xs) = f x (iterList f z xs)

{- The idea is that 'iterList f z' does 'z' on the empty list, and uses
   the function 'f' to combine results in the step case. -}

{- 3.1.1 Summing lists. Using 'iterList' write a function that adds up
   all the elements of a list. HINT: try writing a recursive function
   that does this first and then compare that function with iterList
   above to see how to write sumList using iterList. The notes for
   Lecture 05 on higher-order functions and generating abstract
   versions of functions from concrete ones might be useful. -}

sumList :: [Int] -> Int
sumList = undefined

{- 2 MARKS -}

{- Here is the Tree type again. Trees are built from 'Leaf's and
   'Node's, and each node has two children and a value of type
   'a'. Just as for lists, we can write higher-order functions that
   process trees. -}

data Tree a
  = Leaf
  | Node (Tree a) a (Tree a)
  deriving Show

{- Here is the iterTree function that implements the recursion scheme
   for trees that we'll see in Lecture 08. You will use this function
   below to implement other functions on trees. -}

iterTree :: b -> (b -> a -> b -> b) -> Tree a -> b
iterTree l n Leaf = l
iterTree l n (Node ltree a rtree) =
  n (iterTree l n ltree) a (iterTree l n rtree)

{- 3.1.2 Mapping trees. The 'mapTree' function applies a given function
   to every value stored within the tree, returning the new tree. For
   example

     mapTree (+1) (Node (Node Leaf 1 Leaf) 2 (Node Leaf 3 Leaf))
  ==
     Node (Node Leaf 2 Leaf) 3 (Node Leaf 4 Leaf)

  Define this function using iterTree. -}

mapTree :: (a -> b) -> Tree a -> Tree b
mapTree = undefined

{- 2 MARKS -}

{- 3.1.3 Pretending that lists are trees. Every list can be seen as a
   special kind of lop-sided tree where every node's left subtree is a
   'Leaf'. For example,

     [1,2,3]

   can be represented as:

     Node Leaf 1 (Node Leaf 2 (Node Leaf 3 Leaf))

   Because we can see every list as a tree, we can pretend to offer
   the same interface as 'iterTree', but work on lists instead. This
   is somewhat similar to the "Facade" pattern in OO programming -- we
   are adapting the interface to Trees provided by 'iterTree' to work
   on lists instead.

   Write the 'iterListAsTree' function that takes the same function
   arguments as 'iterTree' but works on lists instead, using the
   translation from lists to trees described above. Note that you
   should do the translation 'on the fly', don't convert the list to a
   tree first. -}

iterListAsTree :: b -> (b -> a -> b -> b) -> [a] -> b
iterListAsTree = undefined

{- 3 MARKS -}

{- 3.1.4 WILL APPEAR IN THE TEST -}
{- 2 MARKS -}


{----------------------------------------------------------------------}
{- 2. COMPARATORS AND SORTING                                         -}
{----------------------------------------------------------------------}

{- The Haskell standard library predefines a type 'Ordering' for
   describing ordering relationships between values:

      > :info Ordering
      data Ordering = LT | EQ | GT
      [...]

   This type is used by the 'Ord' type class. We used the 'Ord' type
   class in Lecture 04 to write sorting functions. If a type is a
   member of the Ord type class we can compare values of that
   type. This is what allows us to use '<' and '>='. The Ord type
   class also defines a function 'compare':

      > :info Ord
      class Eq a => Ord a where
        compare :: a -> a -> Ordering
        [...]

   So 'compare' returns the appropriate result for its two
   arguments. For example:

         > compare 1 2
         LT
         > compare 2 1
         GT
         > compare 1 1
         EQ

   Sometimes, the default ordering for a type is not the one we
   want. For example, we might want to sort the list into descending
   order. We can invert an 'Ordering': -}

invertOrdering :: Ordering -> Ordering
invertOrdering LT = GT
invertOrdering EQ = EQ
invertOrdering GT = LT

{- Now we need to isolate the idea of a thing that compares two values
   of the same type. We will use higher order functions to do this.

   A 'Comparator' in Haskell is a function that takes two values and
   returns an 'Ordering' (satisfying some properties). Let's make a
   type synonym for Comparators: -}

type Comparator a = a -> a -> Ordering

{- Every type that is a member of the 'Ord' type class has a default
   comparator. We just write 'compare' for this, and Haskell's type
   inference mechanism will work out which one to use. However, the
   default comparator might not be the ordering that we wish to
   use. We'll now see how to build new comparators out of old ones.

   (To be a proper comparator, we ought to also have some properties
    for any comparator 'cmp':

      1. cmp x y == invertOrdering (cmp y x)
      2. if (cmp x y == LT) and (cmp y z == LT) then (cmp x z == LT)
      3. if (cmp x y == EQ) then, for all z, (cmp x z == cmp y z)

    We won't get into worrying about these for this exercise though.) -}



{- 3.2.1 Inverting Comparators. Write a function that takes as input a
   comparator and returns a comparator that implements the reverse
   ordering. Use 'invertOrdering'. -}

invert :: Comparator a -> Comparator a
invert = undefined

{- For example:

        > invert compare 1 2
        GT
        > invert compare 2 1
        LT
        > invert compare 1 1
        EQ
-}

{- 1 MARK -}


{- 3.2.2 Transforming Comparators. If we have a 'Comparator a' and a way
   of turning 'b's into 'a's, we can build a 'Comparator b'. Implement
   this: -}

on :: Comparator a -> (b -> a) -> Comparator b
on = undefined

{- For example, to compare pairs on their first element, we might write:

       compare `on` fst :: Ord a => Comparator (a,b)

   Or to compare lists by their length:

       compare `on` length :: Comparator [a] -}

{- 2 MARKS -}


{- 3.2.3 Sorting with a comparator. Here is the 'qsort' function from
   Lecture 04. -}

qsort :: Ord a => [a] -> [a]
qsort []     = []
qsort (x:xs) = qsort smaller ++ [x] ++ qsort larger
  where smaller = [ y | y <- xs, y < x]
        larger  = [ y | y <- xs, y >= x]

{- Rewrite 'qsort' so that it takes as input a 'Comparator a', instead
   of relying on the default one from the 'Ord' instance: -}

qsortWith :: Comparator a -> [a] -> [a]
qsortWith = undefined

{- (Make sure you don't accidentally call 'qsort' in the recursive
   calls!)

   It should be the case that 'qsortWith compare' always gives the same
   answer as 'qsort'. For example:

      > qsortWith compare [5,2,3,4,1]
      [1,2,3,4,5]
      > qsortWith compare ["c", "aaa", "bb"]
      ["aaa","bb","c"]

   But when we use the functions above, we get different orderings:

      > qsortWith (invert compare) [5,2,3,4,1]
      [5,4,3,2,1]
      > qsortWith (compare `on` length) ["c", "aaa", "bb"]
      ["c","bb","aaa"]
-}

{- 5 MARKS -}

{- 3.2.4 WILL APPEAR IN THE TEST -}
{- 2 MARKS -}



{----------------------------------------------------------------------}
{- 3. A PICTURE LANGUAGE                                              -}
{----------------------------------------------------------------------}

{- In this part of the exercise, you will build a small graphics library
   based on the idea of compositing bitmap images.

   A bitmap image can be represented as a function from 'Point's to
   values: -}

type Bitmap a = Point -> a

{- where a point is an (x,y) coordinate: -}

type Point = (Double, Double)

{- Our bitmaps are parameterised by the type of data that can appear at
   each coordinate. For example, to represent bitmaps where we only
   care whether a pixel is set or not, we might use the type:

      Bitmap Bool

   we will use this type as a type of 'masks' for filtering other
   bitmaps.

   For 'real' images, we want every point in the coordinate space to
   be associated with a colour. We will represent colours as their RGB
   components, with an alpha channel for transparency. The type for
   representing colours is: -}

data RGBA = MkRGBA { redChannel   :: Double
                   , greenChannel :: Double
                   , blueChannel  :: Double
                   , alphaChannel :: Double
                   }
  deriving Show

{- where I have named the fields for documentation purposes. Each
   channel is only meant to take values between 0 and 1, and we'll
   have to be careful to enforce that below.

   A colour image is now represented as a value of type:

      Bitmap RGBA

   Let's give ourselves some colours: -}

black :: RGBA
black = MkRGBA 0 0 0 1

white :: RGBA
white = MkRGBA 1 1 1 1

red :: RGBA
red = MkRGBA 1 0 0 1

green :: RGBA
green = MkRGBA 0 1 0 1

blue :: RGBA
blue = MkRGBA 0 0 1 1

{- And the lack of colour (the alphaChannel is set to 0). -}

clear :: RGBA
clear = MkRGBA 0 0 0 0

{- And a colour with opacity: 'opacity f c' makes the colour 'c'
   transparent by a factor of 'f' (which should be between 0 and 1) -}

opacity :: Double -> RGBA -> RGBA
opacity factor (MkRGBA r g b a) = MkRGBA r g b (a * factor)


{- The first picture we'll make is one that 'green' everywhere: -}

greenEverywhere :: Bitmap RGBA
greenEverywhere (x,y) = green

{- See? 'greenEverywhere' is a function that takes a coordinate (x,y)
   and always returns 'green'. A slightly more complex picture is one
   that is blue when the x coordinate is less than 0, and green when
   it is greater than 0: -}

blueAndGreen :: Bitmap RGBA
blueAndGreen (x,y) = if x < 0 then blue else green

{- Just making images like this is all very well, but it is much easier
   to see what is going on if we can look at the pictures we are
   creating. At the bottom of this file, I have defined a function
   'writeBMP' that takes a filename and a 'Bitmap RGBA' and writes it
   to a file in the BMP format. Most image viewers will then be able
   to read this format and display it on screen.

   For example:

       > writeBMP "test.bmp" blueAndGreen

   will write a file called "test.bmp" in the same directory as you
   started GHCi in. Opening this file in an image view will let you
   see the image. Values of type 'Bitmap RGBA' can represent (almost)
   infinitely large images, so 'writeBMP' only takes the coordinates
   in the range (-100,99) in the x and y directions. The origin (0,0)
   is at the centre of the image. -}

{- Constructing pictures directly by writing a function from coordinates
   to colours is possible, but difficult. It is much more fun to build
   pictures up by combining them together. -}


{- 3.3.0 everywhere. Let's start by generalising the 'greenEverywhere'
   picture from above. Write a function that takes a value and returns
   that value at all coordinates. -}

everywhere :: a -> Bitmap a
everywhere = undefined

{- Test your function with 'writeBMP'. 'everywhere red' should generate
   a completely red image, for instance. -}

{- 1 MARK -}


{- 3.3.1 Shapes. To draw shapes, we won't do them using colours
   directly. Instead we will create 'masks' that we will use to 'cut'
   shapes out of other pictures. As mentioned above, a mask is a
   'Bitmap Bool'. We will describe some basic shapes using masks. For
   example, here is a function that generates a circular mask of a
   given radius: -}

circle :: Double -> Bitmap Bool
circle r = \(x, y) -> x*x + y*y <= r*r

{- So 'circle r' assigns 'True' to all points within distance 'r' of the
   origin, and 'False' otherwise. -}

{- Define a function square that takes a size and returns a bitmap
   assigning 'True' to all coordinates within the square of side
   length 'size' centred on the origin, and 'False' otherwise. -}

square :: Double -> Bitmap Bool
square = undefined

{- 1 MARK -}


{- 3.3.2 Colouring in. The shape functions don't return Bitmaps with
   colours, so we can't use 'writeBMP' to look at them
   directly. Instead, we have to translate 'Bool's to actual
   colours.

   Write a function that takes a 'Bitmap Bool' and two colours and
   produces a 'Bitmap RGBA' that uses the first colour when the mask
   is 'True' and the second when it is 'False': -}

colourIn :: Bitmap Bool -> RGBA -> RGBA -> Bitmap RGBA
colourIn = undefined

{- For example,

     > writeBMP "test.bmp" (colourIn (circle 100) green black)

   should give a green circle on a black background. -}

{- 1 MARKS -}


{- 3.3.3 Transforming images, point-by-point. 'colourIn' is an example
   of a function that transforms an image in a fixed way at every
   point. This is a pattern that recurs over and over, so it is worth
   making a higher order function that captures the essence of this
   pattern.

   Define 'mapBitmap' that takes a function from 'a's to 'b's and a
   Bitmap of 'a's and produces a Bitmap of 'b's by applying the
   function at every point. -}

mapBitmap :: (a -> b) -> Bitmap a -> Bitmap b
mapBitmap = undefined

{- It should be the case that:

      mapBitmap (\b -> if b then green else black) (circle 100)

   produces the same image as

      colourIn (circle 100) green black
-}

{- 2 MARKS -}

{- 3.3.4 Transforming two images, point-by-point. 'mapBitmap' is useful,
   but sometimes we want to be able to apply a two argument function
   to two bitmaps simultaneously. Define a function 'liftBitmap' that
   does this: -}

liftBitmap :: (a -> b -> c) -> Bitmap a -> Bitmap b -> Bitmap c
liftBitmap = undefined

{- 1 MARK -}

{- 3.3.5 Varying the function, point-by-point. We could now go on and
   define liftBitmap3, liftBitmap4 and so on for more and more bitmaps
   being combined.

   Instead of doing that, we can define a single function that can be
   used repeatedly. If we allow the function being used to transform
   the image to vary, as well as the argument, then we have a much
   more flexible arrangment. Define a function that takes a bitmap of
   /functions/ and a bitmap of /arguments/ and applys the functions to
   the arguments for each point. -}

bitmapApply :: Bitmap (a -> b) -> Bitmap a -> Bitmap b
bitmapApply = undefined

{- 2 MARKS -}

{- Now we can implement 'mapBitmap' using 'everywhere' and 'bitmapApply':

      mapBitmap f bitmap = everywhere f `bitmapApply` bitmap

   The 'everywhere f' makes a bitmap that has the function 'f' at
   every point, and 'bitmapApply' applies that function to the value
   of 'bitmap' at every point. -}

{- 3.3.6 Re-implementing liftBitmap. Re-implement 'liftBitmap' using
   'everywhere' and 'bitmapApply'. HINT: follow the types! HINT2:
   think of 'bitmapApply' as a funny kind of 'function application
   with extras'. -}

liftBitmap' :: (a -> b -> c) -> Bitmap a -> Bitmap b -> Bitmap c
liftBitmap' = undefined

{- 1 MARK -}

{- 3.3.7 WILL APPEAR IN THE TEST -}
{- 5 MARKS -}

{- Blending. Since we are representing colours with alpha channels for
   transparency, we can overlay one picture on top of another, letting
   the background picture show through the transparent bits of the
   foreground picture. We represent this as the ability to blend RGBA
   colours together. RGBA colours with alpha blending form a monoid:
   we have the completely clear colour 'RGBA 0 0 0 0' and the monoid
   operation is alpha blending. The exact details of alpha blending
   are not important here. See the following URL for a derivation of
   the definition from first principles:

     https://lukepalmer.wordpress.com/2010/02/05/associative-alpha-blending/

   Since we have an associative operation with a unit on RGBA colours,
   we are justified in declaring RGBA an instance of the Monoid
   typeclass: -}
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

{- This is needed for newer versions of GHC. A semigroup is a monoid
   without the 'mempty'. Newer versions of GHC require that every
   Monoid is explicitly also a Semigroup. -}
instance Semigroup RGBA where
  (<>) = mappend


{- This defines '<>' as a synonym for 'mappend'. This
   means we can now write

      colour1 <> colour2

   to blend colour1 and colour2. For example blending 0.5 opacity red
   into green:

      > opacity 0.5 red <> green
      MkRGBA {redChannel = 0.5, greenChannel = 0.5, blueChannel = 0.0, alphaChannel = 1.0}

   yields a colour which is half green and half red, and is fully
   opaque. -}



{- 3.3.8 Blending pictures. Use the 'mappend' function on any Monoid and
   'liftBitmap' to write a function that combines two images. We call
   this function 'over' because it is used to place one picture over
   another. -}

over :: Monoid a => Bitmap a -> Bitmap a -> Bitmap a
over = undefined

{- 1 MARK -}



{- 3.3.9 Cutting out images. A more useful variant of the 'colourIn'
   function is one that takes a mask (a 'Bitmap Bool') and a image (a
   'Bitmap a') and wherever the mask is 'True' uses the image, and
   wherever the mask is 'False' uses the 'mempty' of the monoid. When
   we use the Monoid structure on RGBA, this will correspond to
   leaving the cut-out parts transparent. Define the 'cut' function,
   using 'liftBitmap': -}

cut :: Monoid a => Bitmap Bool -> Bitmap a -> Bitmap a
cut = undefined

{- For example,

      circle 50 `cut` everywhere red

   will produce a circle of radius 50 on a transparent background. -}

{- 1 MARK -}



{- 3.3.10 Space Transformations. All the functions so far have
   concentrated on transforming pixel values individually. Another
   class of transformations is to adjust the coordinate space in some
   way. This allows for rotates, scaling, shearing, flipping of images
   and so on.

   We can represent an arbitrary space transformation as a function of
   type 'Point -> Point'. Write a function that transforms a given
   bitmap by the given transformation: -}

transform :: (Point -> Point) -> Bitmap a -> Bitmap a
transform = undefined

{- 2 MARKS -}


{- With some point transformation functions, we can now create some
   "interesting" pictures: -}

picture :: Bitmap RGBA
picture =
  circle 100 `cut`
   ((circle 50 `cut` everywhere (opacity 0.7 blue))
    `over`
    (transl (50, 50) `transform` (circle 50 `cut` everywhere red))
    `over`
    (transl (-50, -50)
     `transform`
     rotate (pi/4)
     `transform`
     (square 100 `cut` everywhere (opacity 0.5 green))))


transl :: (Double,Double) -> Point -> Point
transl (vx,vy) (x,y) = (x-vx, y-vy)

-- angle in radians
rotate :: Double -> Point -> Point
rotate angle (x,y) = ( x * cos angle - y * sin angle
                     , x * sin angle + y * cos angle)
  

{- 3.3.11 WILL APPEAR IN THE TEST -}
{- 2 MARKS -}


{----------------------------------------------------------------------}
{- 4. PROCESSES                                                       -}
{----------------------------------------------------------------------}

{- This part of the exercise generalises the communicating processes
   from Exercise 2 to allow processes that send and recieve data of
   any type, not just bits. These processes are also a kind of tree,
   except that now the choices after an input are represented by a
   function. These processes can also return a final value, as well as
   the data they output.

   We'll set things up, then it'll be your turn. -}

{- 'Process x a' is the type of processes that send and recieve values
   of type 'x' and terminate with a value of type 'a'.

   For example, we could think of simplified Unix processes that can
   only talk to Standard Input and Standard Output as values of type
   'Process Word8 Int'. They send and recieve 8-bit words
   (i.e. 'char's) and terminate with an int-value exit status. -}
data Process x a
  = End a -- marks the end of a process, returning a value of type
          -- 'a'.
  | Input (x -> Process x a) -- (Input k) requests input of a value
                             -- 'v' of type 'x', and chooses a
                             -- continuation process (k v) based on
                             -- that value.
  | Output x (Process x a) -- (Output v k) outputs a value 'v' of type
                           -- 'x' and continues as the process 'k'.

{- Let's have some example processes. First, the notGate example from
   Exercise 2, rewritten to be a member of the more general CP type: -}

notGate :: Process Bool ()
notGate = Input (\b -> Output (not b) (End ()))

{- See how this is the same as the notGate example in Exercise 2, only
   here instead of explicitly giving the two different options for the
   two possible inputs, we give a function that decides what to do
   instead. In this case, it outputs the 'not' of whatever the input
   is. Using functions instead of explicitly enumerating the cases
   leads to significantly smaller descriptions of processes in most
   cases.

     ASIDE: This switch from using a pair of 'Process' values, one for
     input 'True' and for input 'False', to using a function of type
     (Bool -> Process Bool a) is an instance of a general
     principle. We can think of the different ways of putting together
     types as a kind of 'arithmetic', generalising the idea of
     counting the number of elements of each type. We think of pairing
     as a kind of multiplication: the pair type '(A,B)' is similar to
     'A * B' (or the cartesian product if you know set
     theory). Similarly, the function type 'A -> B' is similar to
     exponentiation 'A^B'. The Bool type, because it has two elements
     ('True' and 'False'), is similar to '2' in this arithmetic of
     types. Therefore, we can have, where '~=' roughly means 'is isomorphic
     to.'

             'Bool -> A' ~= 'A^Bool' ~= 'A^2' ~= 'A * A'

     In the previous exercise, we represented the possible futures of
     a Process after a boolean input as a pair of processes. In this
     exercise, we represent it as a function, using the fact that 'A^2
     = A*A'. The advantage of the function representation is that we
     can now have infinite or very large types for input values, which
     would be impossible or infeasible to represent by pairing. -}

{- Let's have another example process: this process inputs any value,
   and then outputs that same value. Note that this process is
   polymorphic (aka "generic" in OO languages) in the type 'x' of
   input it accepts. -}

echo :: Process x ()
echo = Input (\v -> Output v (End ()))

{- We make processes 'go' in the same way as we did before. We interpret
   them, feeding the 'Input's from a list of inputs, and placing the
   'Output's into a list. There are two main differences with
   'process' from Ex 2: we need to return the extra value attached to
   'End', and we explicitly signal lack of input by using a 'Maybe'
   type. -}

process :: Process x a -> [x] -> (Maybe a,[x])
process (End a)      inputs     = (Just a, [])
process (Input k)    []         = (Nothing, [])
process (Input k)    (v:inputs) = process (k v) inputs
process (Output v k) inputs     = (a,v:outputs)
  where (a,outputs) = process k inputs

{- For example,

      process echo ["Hello"] == (Just (),["Hello"])
-}

{- If we have a process that communicates using 'String's, then we can
   make it actually interact with the user using 'runIO'. This
   function translates process descriptions into I/O commands. This
   function uses Haskell's basic facilites for doing real I/O. We will
   come back to this later in the course. -}

runIO :: Process String a -> IO a
runIO (End a)      = return a
runIO (Input k)    = do { s <- getLine; runIO (k s) }
runIO (Output x k) = do { putStrLn x; runIO k }

{- Here's an example of using 'runIO'. The '>' is the haskell prompt.

        > runIO echo
        hello
        hello

   where the first 'hello' is typed by the user, and the second is
   printed by the computer. You can use runIO to test your processes
   below, interactively. -}

{- Let's make some basic processes that we can use to build larger
   processes. Your job is to write these from their specifications. -}


{- 3.4.0 Define 'input'. 'input' is the process that inputs a single
   value and then ends with that value. -}
input :: Process x x
input = undefined

{- 1 MARK -}



{- 3.4.1 Define 'output'. 'output x' is the process that outputs the
   value x, and then ends with the value (). -}
output :: x -> Process x ()
output = undefined

{- 1 MARK -}



{- 3.4.2 WILL APPEAR IN THE TEST -}
{- 4 MARKS -}



{- 3.4.3 Sequential composition of processes. In the previous exercise,
   sequential composition of processes had type 'Process -> Process ->
   Process'. Here, processes terminate with a value, which is passed
   on to subsequent processes. Define the rest of this function to
   complete the definition of sequential composition of processes.

   Here are some examples of its use:

       > runIO (input `sequ` \x -> output x)
       hello
       hello
       Just ()

       > runIO (input `sequ` \x -> End ())
       hello
       Just ()

   Note that using the the backtick notation to write 'sequ' between
   its arguments allows us to read 'p1 `sequ` \x -> p2' as "do 'p1',
   call the result 'x' and then do 'p2'". -}

sequ :: Process x a -> (a -> Process x b) -> Process x b
sequ (End a)      f = undefined
sequ (Input k)    f = undefined
sequ (Output x k) f = undefined

{- 3 MARKS -}



{- 3.4.4 WILL APPEAR IN THE TEST -}
{- 1 MARK -}



{- 3.4.5 WILL APPEAR IN THE TEST -}
{- 1 MARK -}


{- 3.4.6 Define a process that inputs two numbers and ends with the sum
   of those numbers. -}

addInputs :: Process Int Int
addInputs = undefined

{- 2 MARKS -}


{- 3.4.7 Using the 'sequ' function, define cpApply, which takes a process
   that returns a function, a process that returns a value and returns
   a process that returns the result of applying the function to the
   value. It ought to sequence the operations of the two processes so
   that the process that returns the function goes first. -}

procApply :: Process x (a -> b) -> Process x a -> Process x b
procApply pf pa = undefined

{- 2 MARKS -}


{- 3.4.8 Now write addInputs again, but this time using 'input',
   'procApply', and End. -}

addInputs2 :: Process Int Int
addInputs2 = undefined

{- 1 MARK -}


{- 3.4.9 WILL APPEAR IN THE TEST -}
{- 3 MARKS -}

{- 3.4.10 WILL APPEAR IN THE TEST -}
{- 1 MARK -}

{----------------------------------------------------------------------}
{- END OF EXERCISE                                                    -}
{----------------------------------------------------------------------}


{----------------------------------------------------------------------}
{- APPENDIX                                                           -}
{----------------------------------------------------------------------}

{- Functions implementing BMP file output. These are used by Part 2 of
   the exercise above. -}


{- 'writeBMP filename bitmap' samples 'bitmap' for the pixels in the
   range ((-100,99),(-100,99)) and outputs them as a BMP file with the
   given filename. It uses the 'buildBMP' function defined below to
   construct a 'ByteString Builder' object that describes the stream
   of bytes to write to the file. -}
writeBMP :: FilePath -> Bitmap RGBA -> IO ()
writeBMP filename bitmap = do
  h <- openFile filename WriteMode
  hPutBuilder h (buildBMP 200 200 bitmap)
    `finally` hClose h

{- 'buildBMP width height bitmap' returns a ByteString Builder
   containing the pixels sampled from 'bitmap' around the origin in
   the Windows BMP file format in 8 bits per channel with an 8 bit
   alpha channel. The file format details were taken from here:

      https://en.wikipedia.org/wiki/BMP_file_format#Example_2

   The file format is relatively simple: there is a header describing
   the image (size, resolution, colour layout), followed by the pixel
   data. We are not using any compression. Most of the header is '0'
   because we are just relying on the defaults for colour space
   correction and gamma.

   The 'LE' suffixes on all the word16/32 calls signify that BMP is a
   'little endian' format, as would be expected from its origins on
   Intel x86 systems.

   The sampling and quantization of the bitmap are quite naive. Taking
   the average of surrounding pixels would probably produce "more
   correct" images. -}
buildBMP :: Word32 -> Word32 -> Bitmap RGBA -> Builder
buildBMP width height bitmap = header <> pixelData
  where
    headerSize    = 122
    pixelDataSize = height * width * 4
    fileSize      = headerSize + pixelDataSize

    header =
      fold [ word8 0x42, word8 0x4d -- "BM"
           , word32LE fileSize
           , word16LE 0             -- application specific
           , word16LE 0             -- application specific
           , word32LE headerSize    -- offset to the pixel data
           , word32LE 108           -- DIB header size
           , word32LE width
           , word32LE height
           , word16LE 1             -- 1 colour plane
           , word16LE 32            -- 32 bits per pixel
           , word32LE 3             -- "BI_BITFIELDS" format, no compression
           , word32LE pixelDataSize
           , word32LE 2835          -- horizontal resolution: 2835 ppm (72 DPI)
           , word32LE 2835          -- vertical resolution: 2835 ppm (72 DPI)
           , word32LE 0             -- 0 colours in the palette (not using one)
           , word32LE 0             -- 0 "important" colours
           , word32LE 0x00ff0000    -- red channel bitmask
           , word32LE 0x0000ff00    -- green channel bitmask
           , word32LE 0x000000ff    -- blue channel bitmask
           , word32LE 0xff000000    -- alpha channel bitmask
           , word32LE 0x57696e20    -- "Win " for LCS_WINDOWS_COLOR_SPACE
           , fold (replicate 0x24 (word8 0)) -- CIE colour space endpoints (unused)
           , word32LE 0             -- red gamma (unused)
           , word32LE 0             -- green gamma (unused)
           , word32LE 0             -- blue gamme (unused)
           ]

    pixelData =
      fold [ encode (bitmap (pixelToBitmap x y))
           | y <- [0..height-1]
           , x <- [0..width-1]
           ]

    -- coordinate space transformations
    pixelToBitmap x y =
      ( fromIntegral x - (fromIntegral width / 2)
      , fromIntegral y - (fromIntegral height / 2))

    -- pixel encoding, little endian
    encode (MkRGBA r g b a) = foldMap quantize [ b, g, r, a ]
    quantize v = word8 (round (255 * v))
