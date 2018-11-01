# CS316 "Functional Programming"

Welcome to the webpage for The University of Strathclyde CS316 "Functional Programming"!

This course has a [Twitter account](https://twitter.com/StrathCS316).

*Assessment:* this course is entirely assessed by coursework. There are four exercises that you will complete (details below). For the 2nd, 3rd, and 4th exercises, you will do roughly 60% of the exercise at home or in the labs, and the 40% is done in exam conditions in the lab.

See the [schedule](schedule.txt).

## Contact

**Bob Atkey** LT1305 [robert.atkey@strath.ac.uk](mailto:robert.atkey@strath.ac.uk)

## Lectures

Lectures are at **11am Tuesdays** in LT1415 and **11am Fridays** in LT412.

See the [schedule](schedule.txt) for more details.

Most of the lectures will involve us doing live coding. We will place the code from each lecture in this repository after each lecture, interspersed with commentary covering what we talked about.

- [Lecture 01](lectures/Lec01.hs) : Data and Pattern Matching
- [Lecture 02](lectures/Lec02.hs) : Defining functions
- [Lecture 03](lectures/Lec03.hs) : List comprehensions
- [Lecture 04](lectures/Lec04.hs) : Recursive functions
- [Lecture 05](lectures/Lec05.hs) : Higher-order functions
- [Lecture 06](lectures/Lec06Live.hs) : Declaring types and classes
- [Lecture 07](lectures/Lec07.hs) : QuickCheck
- [Lecture 08](lectures/Lec08.hs) : Recursion Schemes
- [Lecture 09](lectures/Lec09.hs) : Functors and Containers
- [Lecture 10](lectures/Lec10.hs) : Building Pure Evaluators
- Lecture 11 : Monads and Applicatives
- Lecture 12 : Monads we Like
- Lecture 13 : Parser Combinators
- Lecture 14 : More Parser Combinators
- Lecture 15 : Parsing expressions, and Writer and State Monads
- Lecture 16 : Traversing Containers
- Lecture 17 : Infinite Data and Processes
- Lecture 18 : [Previ.se](https://previ.se/) Guest Lecture
- Lecture 19 : Parallelism
- Lecture 20 : Concurrency
- Lecture 21 : A look at Agda (CS410 propaganda)

### Tutorials

In addition to the lectures, there are weekly tutorials at **4pm on Thursdays** in [LT210](http://www.learningservices.strath.ac.uk/avfacilities/roomresults.asp?&menu1=Graham%20Hills&roomField=GH816&findRoom=Show+room+details). These are intended for going through some unassessed homework questions that we will set after the lectures, or for you to ask questions about the assessed exercises.

- [Tutorial 02](tutorials/Tut02.md) : Some questions for the tutorial on Thursday 4th October.

- [Tutorial 05](tutorials/Tut05.md) : Covered `iterList` (like `iterTree` in Exercise 3) and a short introduction to parser combinators.

### One minute papers

At every lecture and tutorial, we will hand out "One minute papers" (OMPs) for you to provide us with feedback on the lecture -- what you have learned in this lecture and what we could have explained better. At the start of the next lecture, we will go through the OMPs from last time and try to address the feedback you give us.

Students registered on the course can see their OMPs on the [Marx system](https://personal.cis.strath.ac.uk/conor.mcbride/shib/Marx/?page=CS316).

## Coursework

As mentioned above, this course is entirely assessed by coursework. The split between the four exercises is shown below:

- Exercise 1 (5%) : [The evaluation game](https://personal.cis.strath.ac.uk/robert.atkey/terms.html). Once you have finished, enter your username and you will get a password. Email this to me (email address above) by the deadline (Thursday 27th September, 4pm).

- Exercise 2 (30%) : [First Order Programming](exercises/Ex2.hs). This was released on Thursday 27th September (week 2), and the final deadline and test are on Monday 15th October (week 5).

- Exercise 3 (30%) : [Higher Order Programming](exercises/Ex3.hs). This was released on Thursday 11th October (week 4), and the final deadline and test are on Monday 5th November (week 8).

- Exercise 4 (35%) : [GHOUL](exercises/Ex4.hs). This was be released on Thursday 1st November (week 7), and the final deadline and test are on Monday 26th November (week 11).

After each of the exercises has been marked, we will email you your marks, and also put them on the [Marx system](https://personal.cis.strath.ac.uk/conor.mcbride/shib/Marx/?page=CS316) for you to see.

### Git commands

To clone a local copy of this git repository, execute

```
git clone https://github.com/bobatkey/CS316-18/
```

## Helpful Links

### Videos

- [What is a Monad? - Computerphile](https://www.youtube.com/watch?v=t1e8gqXLbsU). Graham Hutton (author of the programming in Haskell book linked below) explains Monads.

### The History of Haskell

- [A History of Haskell: Being Lazy With Class](http://haskell.cs.yale.edu/wp-content/uploads/2011/02/history.pdf) is an account of the history of the Haskell language and how it got it's name.

### Other Lecture Courses

These links are to lecture courses by other Universities and companies. You might find them useful as alternative presentations of the material in our course.

- Glasgow uni (free!) <abbr title="Massive open online course">MOOC</abbr> on [Functional programming in Haskell](https://www.futurelearn.com/courses/functional-programming-haskell).

- Video lectures by Erik Meijer on [Functional Programming Fundamentals](https://channel9.msdn.com/Series/C9-Lectures-Erik-Meijer-Functional-Programming-Fundamentals).

- Material from [CIS 194: Introduction to Haskell](http://www.seas.upenn.edu/~cis194/fall16/) at the University of Pennsylvania.

### Books

There are now many books written about Haskell. Here are links to some that we have found useful.

- [Programming in Haskell](http://www.cs.nott.ac.uk/~pszgmh/pih.html) is the book that we have based the first half of this course on. You do not need to buy the book.

- The [Haskell Wikibook](https://en.wikibooks.org/wiki/Haskell). This is a free online book that starts very gently, but also includes some very advanced material.

- [Parallel and Concurrent Programming in Haskell](http://chimera.labs.oreilly.com/books/1230000000929) by Simon Marlow. This book is an excellent description of the facilities in Haskell for parallel and concurrent programming. We will cover some of these in Lectures 20 and 21. The full text is available online for free reading.

- [Learn You a Haskell for Great Good!](http://learnyouahaskell.com/). This is an introductory book on Haskell, covering roughly the same material as this course, but with a different presentation. There are attempts at humour, but you might find them grating after a while. The full text of this book is available online for free.
