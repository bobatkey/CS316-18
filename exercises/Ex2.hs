{-# OPTIONS_GHC -fwarn-incomplete-patterns #-} 
module Ex2Solutions where

import Prelude hiding (words)

{----------------------------------------------------------------------}
{- CS316 (2018/19) EXERCISE 2 : FIRST-ORDER PROGRAMMING               -}
{----------------------------------------------------------------------}

-- Submit by committing to GitLab at or before 2pm on Monday 15th
-- October.  There will be a test on this exercise in the lab on that
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
{- PART 1 : FUN(ctional) WITH LISTS                                   -}
{----------------------------------------------------------------------}

{- 2.1.0 Concatenation of lists. The infix operator ++ concatenates two
   lists. Use it to write a function in pattern matching style which
   concatenates a list of lists. We have given you an unfinished
   definition which you should refine into suitable cases and
   complete. -}

concatLists :: [[x]] -> [x]
concatLists = undefined

{- It may help to think concretely:
   (a) What should
     concatLists [[1], [2,3], [4,5,6]]
   be?

   (b) What should
     concatLists [[2,3], [4,5,6]]
   be?

   (c) How do you combine [1] with the answer to (b) to make the answer
   to (a)?
-}

{- 2 MARKS -}

{- 2.1.1 An odd append. The following function looks a bit like the
   append function we looked at in Lectures 01 and 02, but it is a bit
   different.

   Work out what this function does, and write a non-recursive version
   of it below as the definition of 'oddAppendSpec'.

   HINT: the function `reverse :: [a] -> [a]` may be useful. -}

oddAppend :: [a] -> [a] -> [a]
oddAppend []     ys = ys
oddAppend (x:xs) ys = oddAppend xs (x:ys)

oddAppendSpec :: [a] -> [a] -> [a]
oddAppendSpec = undefined

{- 1 MARK -}

{- 2.1.2 Joining Lists. The 'joinWith' function takes a value 'x' and a
   list of lists 'xs' and concatenates the lists with 'x' placed between
   each list. Examples:

    joinWith 0 [[1,2],[3,4]] == [1,2,0,3,4]
    joinWith False [[True,True],[True],[True]] == [True,True,False,True,False,True]

   Note that the joining value is placed *between* consecutive
   elements, not at either end. Also, because strings are lists of 'Char's,
   this function is also useful for building strings:

    joinWith ':' ["Ty Per", "ty.per@example.com"] == "Ty Per:ty.per@example.com"

   Write 'joinWith': -}

joinWith :: a -> [[a]] -> [a]
joinWith x xs = undefined

{- 3 MARKS -}

{- 2.1.3 Splitting Lists. The function 'splitOn' splits a list at every
   occurence of some value. Examples:

      splitOn 0 [1,2,0,3,0]   = [[1,2],[3]]
      splitOn 0 [1,2,0,3,0,4] = [[1,2],[3],[4]]
      splitOn 0 []            = [[]]
      splitOn 0 [0]           = [[],[]]
      splitOn 0 [0,0]         = [[],[],[]]

   Because strings are lists of 'Char's, 'splitOn' is a useful way of
   breaking down strings:

       splitOn ':' "Ty Per:ty.per@example.com" == ["Ty Per", "ty.per@example.com"]

   Write the function 'splitOnHelper' that 'splitOn' uses to work. -}

splitOn :: Eq a => a -> [a] -> [[a]]
splitOn splitChar xs = splitOnHelper splitChar [] xs

splitOnHelper :: Eq a => a -> [a] -> [a] -> [[a]]
splitOnHelper s group xs = undefined

{- HINT: 'splitOnHelper' works by gathering elements of the input list
   'xs' in 'groups' until it sees an occurence of the splitting
   value. It then returns the current group (reversed back to normal),
   and carries on with a new group. When it runs out of elements to
   process, the last group is reversed and outputted.

   The arguments to 'splitOnHelper' are:

      splitOnHelper s group xs

      - 's :: a'       is the value to split on
      - 'group :: [a]' is the list of values in the current group, in reverse order
      - 'xs :: [a]'    is the input list of elements remaining to be processed. -}

{- 3 MARKS -}

{- 2.1.4 Removing Empties. The function 'splitOn' generates empty lists
   when there are consecutive occurrences of the splitting value. In
   some cases, we don't care about the empty lists. Write a function
   that takes a list of lists, and returns a list only containing the
   non-empty elements in the same order. Examples:

      removeEmpty [[1],[],[2]]         = [[1],[2]]
      removeEmpty [[],[]]              = []
      removeEmpty ["hello","","world"] = ["hello","world"] -}

removeEmpty :: [[a]] -> [[a]]
removeEmpty = undefined

{- 2 MARKS -}

{- 2.1.5 Splitting into words. Using 'splitOn' and 'removeEmpty', write
   a function that splits a string into words. Assume that words are
   separated by spaces ' ' and that words are not empty. Examples:

      words "hello world" = ["hello","world"]
      words "hello   world" = ["hello", "world"]
      words ""              = []
      words "   "           = []
      words "hello, world"  = ["hello,", "world"] -}

words :: String -> [String]
words = undefined

{- 1 MARK -}

{- 2.1.6 WILL APPEAR IN THE TEST. -}
{- 1 MARK -}

{- 2.1.7 Formatting paragraphs.

   In this problem, you will write a function for laying out
   monospaced paragraphs within a fixed margin width. We will
   represent paragraphs as lists of words:

       ["I","will","begin","the","story","of","my","adventures",
        "with","a","certain","morning","early","in","the","month",
        "of","June,","the","year","of","grace","1751,","when","I",
        "took","the","key","for","the","last","time","out","of",
        "the","door","of","my","father's","house."]

   (This is the first sentence of "Kidnapped" by Robert Louis Stevenson:
      https://en.wikisource.org/wiki/Kidnapped_(Stevenson)/Chapter_1 )

   The plan is to turn a list of words into a list of lines by adding
   words to a line with single spaces between them until adding one
   would go over the intended width. As a special case, we always put
   at least one word on each line, even if that would mean going over
   the limit. For example, if the width is 40, then we would get the
   following layout:

     [["I","will","begin","the","story","of","my","adventures"],
      ["with","a","certain","morning","early","in","the"],
      ["month","of","June,","the","year","of","grace","1751,"],
      ... ]

   In this example, the first line is length 39 after putting in the
   spaces. Adding 'with' would take us over the limit, so we start a
   new line.

   We will make the following simplifying assumptions:

     1. Each word takes up the same amount of space as it has
        characters (this is not true for all of the Unicode character
        set, and only really works for ASCII if we assume a monospaced
        font).

     2. Each space character ' ' takes up exactly one space.

   We encode these assumptions into the following function, which you
   will use to keep track of the current column words are being placed
   in. Given a current column 'col' and a word 'word', 'plusCol col
   word' returns the column we'll be in if we add a space and 'word'
   to the line: -}

plusWord :: Int -> String -> Int
plusWord col word = col + 1 + length word

{- Now you write the following function 'layOutLines'. The arguments are
   as follows:

     layOutLines w col line words

     - 'w :: Int' is the target width, use this to determine whether to start
       a new line or not.

     - 'col :: Int' is the current column we are in, it represents how
       wide 'line' is once spaces are added.

     - 'line :: [String]' is the list of words in the current line, in
       reverse order.

     - 'words :: [String]' is the list of words remaining to be added
       to some lines.

   The whole function ought to return the list of lines generated. -}

layOutLines :: Int -> Int -> [String] -> [String] -> [[String]]
layOutLines w col line words = undefined

{- 7 MARKS -}

{- HINT: There are four cases of interest:

   1. We have no words remaining (words is []), and the line is empty.
   2. We have no words remaining, but there is something on this line.
   3. There are some words, but we are at the start of the line.
   4. There are some words, and we are in the middle of a line:
      4(a) the word fits on the line
      4(b) the word doesn't fit on the line

   Case three is important, because otherwise we never lay out a word
   that is longer than a line!

   HINT: the structure of the function is very similar to 'splitOn'
   above, except that the condition used to decide when to split into
   groups is different. -}

{- Once you have written 'layOutParagraph', the following functions will
   start working, which call 'layOutParagraph' with the right initial
   values.

      'formatParagraph 80 kidnapped' should return:

    [["I","will","begin","the","story","of","my","adventures","with","a","certain","morning","early","in","the"],
     ["month","of","June,","the","year","of","grace","1751,","when","I","took","the","key","for","the","last","time"],
     ["out","of","the","door","of","my","father's","house."]]

      'printParagraph 80 kidnapped' should print out:

      λ> printParagraph 80 kidnapped
      I will begin the story of my adventures with a certain morning early in the
      month of June, the year of grace 1751, when I took the key for the last time
      out of the door of my father's house. -}

formatParagraph :: Int -> [String] -> [[String]]
formatParagraph w = layOutLines w 0 []

printParagraph :: Int -> [String] -> IO ()
printParagraph w words = putStrLn (Prelude.unlines (map Prelude.unwords (formatParagraph w words)))

{- The following are two pieces of text that you can use for testing. -}

-- https://en.wikisource.org/wiki/Kidnapped_(Stevenson)/Chapter_1
kidnapped :: [String]
kidnapped = words "I will begin the story of my adventures with a certain morning early in the month of June, the year of grace 1751, when I took the key for the last time out of the door of my father's house."

-- https://en.wikisource.org/wiki/Frankenstein,_or_the_Modern_Prometheus_(Revised_Edition,_1831)/Preface
frankenstein :: [String]
frankenstein = words "The event on which this fiction is founded, has been supposed, by Dr. Darwin, and some of the physiological writers of Germany, as not of impossible occurrence. I shall not be supposed as according the remotest degree of serious faith to such an imagination ; yet, in assuming it as the basis of a work of fancy, I have not considered myself as merely weaving a series of supernatural terrors. The event on which the interest of the story depends is exempt from the disadvantages of a mere tale of spectres or enchantment. It was recommended by the novelty of the situations which it developes ; and, however impossible as a physical fact, affords a point of view to the imagination for the delineating of human passions more comprehensive and commanding than any which the ordinary relations of existing events can yield."


{----------------------------------------------------------------------}
{- PART II : CURSORS                                                  -}
{----------------------------------------------------------------------}

{- The following datatype represents a 'pointer' into, or 'cursor' in, a
   list. It allows us to edit the middle of the list within having to
   search all the way down from the head each time. In this section,
   we will slowly build up to having a rudimentary line based text
   editor. -}

data Cursor a
  = Within [a] a [a]
  | AtEnd [a]
  deriving Show

{- The 'Cursor' datatype has two constructors:

      'Within before point after'
        -- represents a cursor in the middle of the list
          - 'before' is the content of the list before the cursor, in reverse order
          - 'point'  is the item under the cursor
          - 'after'  is the content of the list after the cursor, in normal order

      'AtEnd before'
        -- represents a cursor just after the end of a list
          - 'before' is the content of the list before the cursor

   Examples:

      Within [2,1] 3 [4,5]  represents   [1,2, 3 ,4,5]
                                              ^^^

      Within [1] 2 [3,4,5]  represents   [1, 2 ,3,4,5]
                                            ^^^

      Within [] 1 [2,3,4,5] represents   [ 1 ,2,3,4,5]
                                          ^^^

      AtEnd [5,4,3,2,1]     represents   [1,2,3,4,5]
                                                    ^^^

   We will be particularly interested in cursors pointing into lists
   of characters. Because strings are lists of characters, they get
   special treatement in Haskell. Here are some examples, where the
   location of the cursor is represented by square brackets [-].

      Within "eh" 'l' "lo"  represents he[l]lo
      AtEnd "olleh"         represents hello[_]
      Within "" 'h' "ello"  repersents [h]ello

   The function 'displayCursor' gives an ASCII art rendering of a
   cursor over c. Try it out in GHCi to see how it represents different
   cursor positions, and to get yourself familiar with the cursor
   representation. -}

displayCursor :: Cursor Char -> String
displayCursor (AtEnd before)              = oddAppend before "[_]"
displayCursor (Within before point after) = oddAppend before ('[':point:']':after)

{- 'toCursor' converts a list into a cursor, placing the cursor at the
   start of the list. Try it out in GHCi, along with the displayCursor
   function. -}

toCursor :: [a] -> Cursor a
toCursor []     = AtEnd []
toCursor (x:xs) = Within [] x xs

{- 'fromCursor' forgets the position of the cursor in a list and returns
   the list. Again, try this function out in GHCi to familiarise
   yourself with the cursor representation. Try lots of different
   examples. -}

fromCursor :: Cursor a -> [a]
fromCursor (AtEnd before)              = oddAppend before []
fromCursor (Within before point after) = oddAppend before (point:after)


{- 2.2.0 Reading the element at cursor. Write a function that returns the
   element currently under the cursor. If there is no such element, then
   it should return 'Nothing'. -}

getPoint :: Cursor a -> Maybe a
getPoint cursor = undefined

{- 1 MARK -}

{- 2.2.1 Movement. Here is a function that moves the cursor one step to
   the right. Note that there are three cases:

     1. If we are already at the end ('AtEnd'), we do nothing and
        return the cursor as is.

     2. If we are one before the end (the 'after' list is empty), we
        become 'AtEnd', moving the point into the head of the 'before'
        list.

     3. If we are in the middle, we move the point into the 'before'
        list, and take the head of the 'after' list as the new point.

   This definition should make it clear why we represent the 'before'
   list in reverse: it makes moving the cursor into a quick operation
   of prepending elements on to lists. -}

moveRight :: Cursor a -> Cursor a
moveRight (AtEnd before)                  = AtEnd before
moveRight (Within before point [])        = AtEnd (point:before)
moveRight (Within before point (a:after)) = Within (point:before) a after

{- Have a play with this function in GHCi to experiment with how it
   works.

   Now you write the 'moveLeft' function. There will be four cases:

    1. The cursor is at the end and start of an empty line
    2. The cursor is at the end of a non-empty line
    3. The cursor is at the start of a non-empty line
    4. The cursor is within a non-empty line.

   Turn these cases into Haskell patterns and work out what to do in
   each case.

   Some examples:

     moveLeft (AtEnd [])            == AtEnd []
     moveLeft (AtEnd [4,3,2,1])     == Within [3,2,1] 4 []
     moveLeft (Within [] 1 [2,3,4]) == Within [] 1 [2,3,4]
     moveLeft (Within [2,1] 3 [4])  == Within [1] 2 [3,4]

   A helpful thing to remember is that moveLeft (like moveRight)
   should not alter the content of the cursor in any way. More
   formally, for all cursors 'c', 'fromCursor c == fromCursor
   (moveleft c)'. -}

moveLeft :: Cursor a -> Cursor a
moveLeft cursor = undefined

{- 3 MARKS -}

{- 2.2.2 Inserting Text. 'moveRight' and 'moveLeft' do not alter the
   content of the cursor. Now you will write a function that does edit
   the text. 'insert x cur' should insert the value 'x' "before" the
   cursor (in a similar way to pressing a key inserts a character
   "before" the cursor in your text editor). Examples:

      insert 5 (AtEnd [3,2,1])      == AtEnd [5,3,2,1]
      insert 5 (Within [2,1] 3 [4]) == Within [5,2,1] 3 [4]
-}

insert :: a -> Cursor a -> Cursor a
insert x cursor = undefined

{- 2 MARKS -}

{- 2.2.3 WILL APPEAR IN THE TEST. -}
{- 2 MARKS -}

{- 2.2.4 WILL APPEAR IN THE TEST. -}
{- 2 MARKS -}

{- 2.2.5 Deletion. Write a function that deletes the element under the
   cursor (similar to pressing the 'delete' key in a text editor (not
   the backspace key!)). If there is no element under the cursor, then
   nothing happens. If there is any element to the right of the
   cursor, it is used to fill in the gap left. Examples:

     delete (AtEnd [3,2,1])        == AtEnd [3,2,1]
     delete (Within [3,2,1] 4 [])  == AtEnd [3,2,1]
     delete (Within [3,2,1] 4 [5]) == Within [3,2,1] 5 []
-}

delete :: Cursor a -> Cursor a
delete cursor = undefined

{- 2 MARKS -}

{- 2.2.6 WILL APPEAR IN THE TEST.  -}
{- 3 MARKS -}

{- 2.2.7 WILL APPEAR IN THE TEST. -}
{- 2 MARKS -}

{- 2.2.8 WILL APPEAR IN THE TEST. -}
{- 3 MARKS -}



{- Once you have some or all of the functions above written, you will be
   able to use them as a simple text editor. Running

     λ> editor "Hello"
     [H]ello

   starts the editor and displays a cursor. Commands are entered by
   typing them and pressing 'Enter'. The commands are:

      'q'  -- quits
      'r'  -- move right
      'l'  -- move left (needs the moveLeft function written)
      'iX' -- inserts 'X' (needs the 'insert' function)
      'x'  -- deletes the character under the cursor (needs 'delete')

    An example:

      λ> editor "Hel;o"
      [H]el;o
      r
      H[e]l;o
      r
      He[l];o
      r
      Hel[;]o
      x
      Hel[o]
      il
      Hell[o]
      q
      "Hello"
-}

data Result a
  = Continue a
  | Stop
  | Error
  deriving Show

decode :: String -> Cursor Char -> Result (Cursor Char)
decode "q"        cursor = Stop
decode "l"        cursor = Continue (moveLeft cursor)
decode "r"        cursor = Continue (moveRight cursor)
decode ['i',c]    cursor = Continue (insert c cursor)
decode "x"        cursor = Continue (delete cursor)
decode _          cursor = Error

editor :: String -> IO String
editor string = go (toCursor string)
  where
    go cursor = do
      putStrLn (displayCursor cursor)
      cmd <- getLine
      case decode cmd cursor of
        Stop            -> return (fromCursor cursor)
        Continue cursor -> go cursor
        Error           -> do putStrLn "???"; go cursor


{----------------------------------------------------------------------}
{- PART 3 : REPRESENTING PROCESSES                                    -}
{----------------------------------------------------------------------}

{- This exercise is about modelling processes which input and output
   bits. Processes are things. They're a kind of tree, representing a
   decision process, given by the following datatype. -}

{- We'll do the setup, then it'll be your turn. -}

data Process
  = End    -- marks the end of the process, so no more input or output
  | Output Bool Process
           -- (Output b p) outputs bit b, then continues as p
  | Input Process Process
           -- (Input pt pf) inputs a bit, continuing as pt if it's
           -- True, pf if False
  deriving Show

{- Don't expect the data in this type to *do* anything! Rather, they
   *represent* processes. We'll see how to interpret them shortly.

   Let's have an example process: this process should output False if its
   input is True and True if its input is False. -}

notGate :: Process
notGate = Input (Output False End) (Output True End)

{- See? If the input is True, we take the left path and find (Output
   False End), otherwise we go right and find (Output True End).
   Either way, we make one output and then stop.

   How can we make processes go? We need to interpret them. Here's
   how.  The "process" function takes a Process to interpret, and a
   list of input bits in [Bool], then produces the list of output
   bits. -}

process :: Process -> [Bool] -> [Bool]
process End            bs        = []
  -- when we're at the end, there is no more output
process (Output b p)   bs        = b : process p bs
  -- the output from (Output b p) had better begin with b, and the rest
  -- is whatever comes out from running p on the input
process (Input tp fp)  (b : bs)  = process (if b then tp else fp) bs
  -- when the process wants input, the result depends on the first bit
  -- in the input list: if that's True, then we continue with the tp
  -- branch; if it's false, we continue with the fp branch. In both
  -- cases, we feed the rest of the input bs to the continuing process
process (Input tp fp)  []        = []
  -- in the unfortunate case where the process wants input but the input
  -- list is empty, we're a bit stuck; let's stop and return no output

{- Let's try it out. Here are some test examples. Try loading this file
   in ghci, then evaluating testNotT and testNotF at the prompt. Do
   you get what you expect? -}

testNotT :: [Bool]
testNotT = process notGate [True]

testNotF :: [Bool]
testNotF = process notGate [False]

{- 2.3.0 Outputting a single bit. Write a function that takes a boolean
   value and returns a process that outputs that bit and ends. You
   should have:

      process (output True) [] = [True]

   and correspondingly for False. -}

output :: Bool -> Process
output b = undefined

{- 1 MARK -}

{- 2.3.1 Copycat. Write a definition of a process, similar to the
   notGate, that reads its input and outputs it unaltered. You should
   have:

     process copyCat [True]   =  [True]
     process copyCat [False]  =  [False]
-}

copyCat :: Process
copyCat = undefined

{- 1 MARK -}

{- 2.3.2 Outputting multiple bits. Write a function that takes a list of
   bits and generates a process that outputs all of them, in
   order. You should have:

      process (outputs [True,False,True,True]) [] == [True, False, True, True]

   and so on. -}

outputs :: [Bool] -> Process
outputs bs = undefined

{- 2 MARKS -}

{- 2.3.3 Duplication. Write a process that inputs one bit, and then
   outputs it *twice*. You should have:

      process duplicate [True]  == [True,True]
      process duplicate [False] == [False,False]
-}

duplicate :: Process
duplicate = undefined

{- 1 MARK -}

{- 2.3.4 AND and OR gates. Write processes that act like an AND gate and
   an OR gate. You should have:

      process andGate [True,True]   == [True]
      process andGate [False,True]  == [False]
      process andGate [True,False]  == [False]
      process andGate [False,False] == [False]

      process orGate [True,True]   == [True]
      process orGate [False,True]  == [True]
      process orGate [True,False]  == [True]
      process orGate [False,False] == [False]
-}

andGate :: Process
andGate = undefined


orGate :: Process
orGate = undefined

{- 1 MARK -}

{- 2.3.5 WILL APPEAR IN THE TEST. -}
{- 3 MARKS -}

{- 2.3.6 Sequencing processes. Write a function which combines two
   processes in sequence, so that the second begins once the first has
   ended.  That is, you should 'graft' the second process in place of
   all the End markers in the first process. HINT: the structure of
   this function is very similar to 'append'. -}

sequ :: Process -> Process -> Process
sequ p q = undefined

{- To check that you've got it right, make sure that

   process (sequ notGate notGate) [True,True]   = [False,False]
   process (sequ notGate notGate) [True,False]  = [False,True]
   process (sequ notGate notGate) [False,True]  = [True,False]
   process (sequ notGate notGate) [False,False] = [True,True]

   That is, sequencing two notGate components gives you a process
   which negates two inputs. -}

{- 3 MARKS -}

{- 2.3.7 WILL APPEAR IN THE TEST. -}
{- 3 MARKS -}

{- 2.3.8 Piping one process into another. Write a function which
   combines two processes so that the output from the first is used as
   the input for the second.  That is, the combined process should
   keep the inputs from the first process and the outputs from the
   second process, but hide the communication in the middle. Give
   priority to the second process, so the first runs only when the
   second is demanding input. We've done some of it for you, but you
   may still need to refine the pattern match further.

   You should have:

     process (pipe notGate notGate)                  [True]  == [True]
     process (pipe notGate notGate)                  [False] == [False]
     process (pipe duplicate (sequ copyCat notGate)) [False] == [False, True]
-}

pipe :: Process -> Process -> Process
pipe p1            End           = End
pipe p1            (Output b p2) = Output b undefined -- what happens next?
pipe End           (Input t f)   = End
  -- the second process is hungry, but it starves to death!
pipe (Output b p)  (Input t f)   = undefined  -- what goes here?
  -- communication: the first process is ready to output, the second
  -- wants to input, so the output from the first should determine
  -- what happens next, somehow
pipe (Input t1 f1) (Input t2 f2) =
  Input undefined undefined -- what happens in each case?
  -- the second process is hungry, and so is the first, so ask 'the world'
  -- for some input

{- 5 MARKS -}

{----------------------------------------------------------------------}
{- END OF EXERCISE                                                    -}
{----------------------------------------------------------------------}
