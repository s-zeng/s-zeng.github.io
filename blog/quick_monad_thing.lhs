% A quick monad thing

---
author: Simon Zeng
date: \today
---

```haskell
import Control.Monad
-- Abraham Lincoln, 2019
```

I recently wrote a bunch of code at work today that was a relatively common real worl use of monads. Thought I'd share here.

By the way, this is both a valid pandoc markdown file AND a haskell source file! You can run with `ghci <this_file>` or compile to pdf with `pandoc -o output.pdf <this_file>`

Suppose you want to do the following:

1. Take an integer as input
2. Verify that it's a certain form (i.e. a multiple of 7 maybe) (in real world, this was verifying a checksum)
3. Check that a corresponding element exists in a database
4. Verify that the value attached to that element in the database is also a valid form

In something like python, you would probably do something like this all day:

```python3
def f(x):
    if valid_input(x):
         if x in database:
              value = database[x]
              if valid_output(value):
                   return value
    raise SomeException
```

You can probably see that as checks and conditions and complexity grows with this kind of thing, it can quickly get unwieldy.
The natural response for this in langauges like Python, of course, is to ask for forgiveness rather than permission: just do the computations first and catch exceptions later.

The problem with that approach, especially with larger code bases, and multiple engineers working on the same codebase, is that it becomes increasingly difficult to tell when something will throw an error, which errors will be thrown, when to catch errors (are these errors already handled lower/higher on the stack?), and in particular, it's almost impossible to guarantee that your code will not crash.

In Haskell, we are a statically typed language. Everything that a function does should have a type signature that corresponds to what to expect.
Say you have a constant integer in haskell. What would that look like?

```haskell
m :: Int
m = 5
```

In Java, for comparison, that would be

```java 
int m = 5;
```

But in Java, what about an integer that you read from stdin? It would also just have a type signature of `int`.
Haskell is special in that since `m :: Int` looks like a constant, you would need some special type to denote that x is not a constant, for instance, `x :: IO Int`

Back to our earlier example: in Haskell, throwing exceptions willy nilly is bad practice.
You want your type signatures to represent something that is failable.
Because ultimately, with a properly constructed type definition, then successful compilation => never crashing!!!

So for our example monad, we have a data type that can either hold the successful result of a computation, or an object describing an error that has occured.

```haskell
data Exceptable a = Error String | Result a deriving (Show)
```

For this to be a useful exception type, it should be something that essentially allows you to run functions on it as normal if we got a valid result.
But when there's an error, it should carry that error through to the end.

```haskell
instance Monad Exceptable where
    return = Result
    (Error  x) >>= f = Error x
    (Result y) >>= f = f y
```

For compatibilty reasons, I also need to define the Applicative and Functor instance.
I won't explain those for now, but you can look it up for interest. 

```haskell
instance Applicative Exceptable where
    pure  = return
    (<*>) = ap
instance Functor Exceptable where fmap f x = x >>= (return . f)
```

Now, back to monads.
The core of monadic programming style, are functions of the form `(a -> m b)`.
The secret is that monads make it extremely easy to chain and sequence functions of that form together. So it is extremely easy for separate developers to build smaller building blocks, and then compose our way up to bigger building blocks.
For instance, we can turn each of our 4 steps into a separate (a -> m b) function, and then chain it all together.

(note: I'm gonna write really contrived versions of what actually happened just for the sake of simplicity and time)

```haskell
validate_input :: Int -> Exceptable Int
validate_input x = if x `mod` 7 == 0 then Result x else Error "Bad input"

{-
This was verifying that the entry reference format was valid in the real world
So if it was valid, it would return it as a result. If it was invalid, throw error.
(in real world we have some higher level cleaner ways of doing this kind of check on booleans,
but the real world version wasn't a simple boolean check anyway so this is just for simplicity)
-}
valid_reference :: Int -> Exceptable Int
valid_reference 14 = Result 14
valid_reference 21 = Result 21
valid_reference x  = Error "Bad database reference"

-- pretend that this was something that read from database
retrieve_from_database :: Int -> Exceptable String
retrieve_from_database 14 = Result "data!"
retrieve_from_database x  = Error "Database permissions error"
```

So now, with these (a -> m b) functions in hand, it's time to chain them!
Suppose we got ourselves some input data that came from elsewhere in the program:

```haskell
input :: Exceptable Int
input = Result 5
```

Then we can bind it to all of our smaller components to get what we want!

```haskell
output :: Exceptable String
output = input >>= validate_input >>= valid_reference >>= retrieve_from_database
-- -> output 
-- Error "Bad input"
```

So we were able to do what the big if statement chain did in python, but in practically one line! 
In order to achieve the same level of run time safety in Python, you would need all sorts of nesting, or stack management shenanigans. Lots of layers.
The joy of monads is that they are able to keep everything flat!
The input to (>>=) is (m a). The output of (>>=) is a (m b). Nowhere do we have (m (m (m a))) or whatever.
By keeping things flat, and making outputs of >>= the same as the input of bind, it becomes possible to chain lots of smaller components together in one go.
Composability is key!

For instance, we could have combined input sanitation and format validation in one go:

```haskell
validate_input_and_format :: Int -> Exceptable Int
validate_input_and_format = validate_input >=> valid_reference
-- (>=>) is (.) but for functions (a -> m b) instead of (a -> b)
-- f >=> g = \x -> f x >>= g
```

So our output computation can be a bit smaller:

```haskell
output2 :: Exceptable String
output2 = input >>= validate_input_and_format >>= retrieve_from_database
```

Or, we could just make output a function directly! like so...

```haskell
output_func :: Int -> Exceptable String -- note the (a -> m b) form!
output_func input = validate_input input >>= valid_reference >>= retrieve_from_database
```

...which is equivalent to...

```haskell
output_func2 :: Int -> Exceptable String
output_func2 = validate_input >=> valid_reference >=> retrieve_from_database
```

so now we can just run:

```
-> output_func2 7
Error "Bad database reference"
-> output_func2 14
Result "data!"
```

So as we get non-error data from other parts of the program, we can just >>= that data into output_func and get more data. Composability of effectful actions is the core idea of monads.

I ask you again to consider what the python equivalent of this would be.
To get this kind of simplicity, flatness, and composability in Python, you would need a lot of exception throwing and catching (since if statement chains can't get you this level of flatness).
And that just gets messy in large scale.
Sure, if you were to make something similar to this super small program in that manner, then you would be fine.
But with large code bases, that quickly breaks down.
Monads let you keep things clean and flat, no matter how large your code gets.

There's a lot more I can talk about monads. There is another reason that they're used - they have a strong theoretical background from category theory.
A lot of higher level control structures of monads are lifted straight from once-theoretical category theory constructs.
By using the math so directly, we benefit from all the theorems and results that they provide.

A lot more things are monads than one may realize. For example, lists are monads:

```
instance Monad [] where
    return x = [x]
    lst >>= f = concatMap f lst
-- (note: concatMap is just flatMap (a.k.a. map function, then flatten list))
```

And list comprehensions are just monad structures!

```haskell
-- for example, a basic comprehension:
comprehension_a = [(x, y) | x <- [3, 4, 5], y <- [1, 2, 3], x + y == 6]
-- has the following monadic form:
comprehension_b = [3, 4, 5] >>= \x -> [1, 2, 3] >>= \y -> guard (x + y == 6) >> return (x, y)
-- in haskell, we have syntax sugar for this called do-notation:
comprehension_c = do
    x <- [3, 4, 5]
    y <- [1, 2, 3]
    guard (x + y == 6)
    return (x, y)
-- where ([letter] <- [val]) is sugar for [val] >>= \[letter] -> ...
-- and every other line has implicit `>>` between them
```

Another surprising one: functions are monads! If you know continuation passing style, then I can tell you that CPS is equivalent to the monad instance of functions. In pseudo-haskell,

```
instance Monad (r ->) where
    return x = \_ -> x
    f >>= g  = \r -> g (f r) r
```

Why is this the implementation you may ask? Because this is the only implementation that follows the monad laws from math. That's beyond the scope of this right now though

So since so many things are monads, that lets us write really general code as control flow stuctures!
For example:

```haskell
generalized_cartesian :: Monad m => m a -> m b -> m (a, b)
generalized_cartesian xs ys = do
    x <- xs
    y <- ys
    return (x, y)

-- > generalized_cartesian [1, 2] [3, 4]
-- -> [(1, 3), (1, 4), (2, 3), (2, 4)]
-- > generalized_cartesian (Result 3) (Result 4)
-- -> Result (3, 4)
-- And of course, the function monad :)
-- > generalized_cartesian (\x -> x*x) (\x -> x+x) $ 5
-- -> (25, 10)
```

In general, monads let you structure a potentially stateful computation, by letting you separate the context/structure of a computation from its execution.
In principle, its possible to write a monad such that the following do block is valid code and behaves like an assembly language:

```
do
    add 3 0 3
    mov 3 4
    label "asdf"
    jlz 3 "asdf"
```

Implementation is an exercise for the reader :). 

So long as every monad instance follows the monad laws, then we achieve ultimate control over state, effects, and the order in which things are run.
We can do so in such a manner that avoids aribtrary control flow (who knows where you'll end up when you throw an exception???),
that is statically typed (you can tell when something can error/change state just from the type!!!),
and that can be always flat and doesn't need to nest (unlike big if-chains!).
Furthermore, the same mathematical structure that allows you to do this has applications in many more areas that allow us to write really really general combinators that can sequence computations of any kind, as long as they are monads!

Sorry if this doesn't make sense it's kinda late when I'm writing this lmao.
But that's the gist of why monads are nice.
Don't hesitate to ask me any questions if you have any.
