CIS 552 Lecture 1
=================

A literate Haskell file is one where the file name ends in .lhs, and
all lines prefixed by '>' are Haskell source code. All other lines are
ignored by the Haskell compiler. The
[html](http://www.cis.upenn.edu/~cis552/current/lectures/Lec1.html) is
generated directly from the
[lhs](http://www.cis.upenn.edu/~cis552/current/lectures/Lec1.lhs) version
of the lecture notes. Feel free to download the source code and play
with it yourself after class. You'll need to turn in a version of it
for the zeroth homework assignment.

Every Haskell file begins with a few lines naming the module (must
start with a capital letter and be the same as the file name) and
(optionally) importing definitions from other modules.

> module Lec1 where
> import Test.HUnit      -- import unit testing stuff

Programming in Haskell is about substituting equals by equals
-------------------------------------------------------------

      3 * (4 + 5)

      { by addition }

      3 * 9

      { by multiplication }

      27

That's it!






What is Abstraction?
--------------------

Pattern Recognition

      31 * (42 + 56)

      70 * (12 + 95)

      90 * (68 + 12)





Generalize to a function by defining an equation
 
> pat x y z = x * (y + z)

The important question is not "What does this function do?"
but, instead "What does this function mean?" We can reason
about that meaning using what we know about equality.


     pat 31 42 56

     { function call } 

     31 * (42 + 56) 

     { addition }

     31 * 98

     { multiplication }

     3038


Functions, like `pat`, are the core abstraction mechanisms in functional
programming. 


The GHC System
--------------

Iteractive shell "ghci"

     :load Lec1.hs
     expression
     :type expression (:t)
     :info variable   (:i)

Emacs with haskell-mode              

     load file ^C-c ^C-l

Package management: "cabal"

Download and install libraries:

     cabal install hunit






Elements of Haskell
-------------------

* Everything is an *expression*
* Expressions evaluate to *values* 
* Every expression has a *type*







Basic types 
-----------

> bigInt = 31 * (42 + 56)  :: Int  -- word-sized integers

> ii = 31 * (42 + 56) :: Integer   -- arbitrarily large integers

> d = 3.1 * (4.2 + 5) :: Double    -- double precision floating point

> c = 'a' :: Char 

> b = True :: Bool 

(Note that numbers, `+`, and `*` are overloaded) 








Function types
--------------

      A -> B

Function taking an input of type `A` and yielding an output of type
`B`

> pos :: Int -> Bool
> pos x = x > 0









Multi-argument function types
-----------------------------

       A1 -> A2 -> A3 -> B

Function taking inputs of type `A1`, `A2`, and `A3` and returning a
result of type `B`

> arith :: Int -> Int -> Int -> Int
> arith x y z = x * (y + z)






Symbolic vs. alphabetic names
-----------------------------

Note that parens around a symbolic (infix) name turn it into a regular
name.

> plus :: Int -> Int -> Int 
> plus = (+)

> p0 :: Int
> p0 = (+) 2 ((*) 3 4)


Likewise we can use alphabetic name in backquotes as infix.

> p1 :: Int
> p1 = 2 `plus` 2






Tuples
------

        (A1, ..., An)

Ordered sequence of values of type `A1`, .. `An` 

> t1 = ('a', 5)          :: (Char, Int)
> t2 = ('a', 5.2, 7)     :: (Char, Double, Int)
> t3 = ((7, 5.2), True)  :: ((Int, Double), Bool)

The structure must match the type.



Extracting values from tuples 
-----------------------------

Pattern Matching extracts values from tuples

> pat' :: (Int, Int, Int) -> Int
> pat' (x, y, z) = x * (y + z)


> patEval = pat' (1,2,3)




Optional values
---------------

        Maybe A

Either a value of type `A`, or else nothing. 

> m1 :: Maybe Int
> m1 = Just 2

> m2 :: Maybe Int
> m2 = Nothing






Extracting values from 'Maybe's 
--------------------------------

Pattern Matching extracts values from maybes; need a pattern for each
case.


> pat'' :: Maybe Int -> Int
> pat'' (Just x) = x
> pat'' Nothing  = 2




Patterns can be nested, too.

> join :: Maybe (Maybe a) -> Maybe a
> join (Just (Just x)) = Just x
> join (Just Nothing) = Nothing
> join Nothing = Nothing

A slightly simpler way to write the same thing:

> join' :: Maybe (Maybe a) -> Maybe a
> join' (Just x) = x
> join' _ = Nothing




'Maybe' is useful for partial functions
---------------------------------------

> location :: String -> Maybe String
> location "cis501" = Just "Wu & Chen"
> location "cis502" = Just "Heilmeier"
> location "cis520" = Just "Wu & Chen"
> location "cis552" = Just "Towne 311"
> location _        = Nothing    -- wildcard pattern, matches anything






Homework Zero
-------------

First, follow the [homework instructions
online](http://www.cis.upenn.edu/~cis552/homework0.html). Then replace
`undefined` below with your answers.

> name :: String
> name = "Jae Joon Lee"

> pennkey :: String
> pennkey = "jjlee"

Your github userid. 

> githubUserId :: String
> githubUserId = "dlwownssla17"

Write a few sentences about your programming background. What
languages have you used before? What is the size of the largest
programs you have written in those languages?

> programmingExperience :: String
> programmingExperience = "I have experience with Java, C, OCaml, Python, \
>                         \Ruby, and Bash, as well as web programming \
>                         \(HTML/CSS/Javascript/Python and Flask/\
>                         \Ruby on Rails). The largest program I have \
>                         \written individually was an adventure game \
>                         \in Bash with around 13,000 lines of code."

What do you hope to get from CIS 552?  What do you expect to learn
from the course?

> desiredOutcome :: String
> desiredOutcome = "I hope to further my appreciation of functional \
>                  \programming language and learn better practices of \
>                  \an advanced and experienced programmer."



Obligatory Hello world
----------------------

Note, we've gotten this far without doing any I/O. That's pretty
standard in Haskell. Working with GHCi means that we can see the
answers directly, we don't need an action to print them out. However,
a standalone executable needs to do *something*, so we demonstrate
that here.

The batch compiler "ghc" compiles and run large programs.  There must be a
definition of 'main' somewhere.  

     ghc -o Lec1 --make -main-is Lec1 Lec1.lhs 

> main :: IO ()
> main = putStrLn ("Hello " ++ name)

(There can be multiple source files, and if the one that includes
'main' is called 'Main.lhs' you can leave off the '-main-is' flag.)
