# Somehow chaotic and short introduction/overview to Haskell

## History

Named after Haskell Curry and rest is too boring, so let's skip it.

## How to get it?

https://www.haskell.org/

## This talk

https://github.com/sielakos/haskell-talk

You can run examples with docker or just install haskell or whatever

## Usage

### interactive

```console
$ ghci
GHCi, version 7.10.3: http://www.haskell.org/ghc/  :? for help
Prelude>
```

### compilation

example program

```haskell
-- examples/program1.hs
import System.Environment
import Data.List
import Control.Monad

main = do
  args <- getArgs
  -- mapM_ is like map, but for also accumulates monads
  mapM_ (\arg -> print $ sum [1 .. read arg]) args
```

```console
$ ghc --make examples/program1.hs -O
[1 of 1] Compiling Main             ( examples/program1.hs, examples/program1.o )
Linking examples/program1 ...
$ ./examples/program1 5 10 15 10000
"15"
"55"
"120"
"50005000"
```

### build system

There is also build system for haskell [cabal](https://www.haskell.org/cabal/)

## What the hell is that Haskell anyway?

So according to Wiki:

Haskell is a standardized, general-purpose purely functional programming language, with non-strict semantics and strong static typing.

**But what the hell does terms mean anyway?**

So...

### purely functional

mathematical functions, immutable data and no side effects.

```haskell
-- examples/example1.hs
addOne::Int -> Int
addOne x = x + 1

hypotenuse::Floating a => a -> a -> a
hypotenuse a b = (a**2 + b**2)**(1.0/2)

hypo3::Floating a => a -> a
hypo3 = hypotenuse 3

applyToList f = map f [1..10]
```
Let's see it in action

```console
$ ghci
GHCi, version 7.10.3: http://www.haskell.org/ghc/  :? for help
Prelude> :load examples/example1.hs
[1 of 1] Compiling Main             ( examples/example1.hs, interpreted )
Ok, modules loaded: Main.
*Main> addOne 3
4
*Main> :t addOne
addOne :: Int -> Int
*Main> :t addOne 5
addOne 5 :: Int
*Main> hypotenuse 3 4
5.0
*Main> :t hypotenuse 4
hypotenuse 4 :: Floating a => a -> a
*Main> hypo3 4
5.0
*Main> applyToList hypo3
[3.1622776601683795,3.605551275463989,4.242640687119285,5.0,5.830951894845301,6.708203932499369,7.615773105863909,8.54400374531753,9.486832980505138,10.44030650891055]
*Main> :t applyToList
applyToList :: (Enum a, Num a) => (a -> b) -> [b]
```

Also valid method of loading file

```console
$ ghci examples/example1.hs
```

### non-strict semantics

lazy execution, nothing is evaluated until needed and it's done only once.

```haskell
-- examples/example2.hs
primes = filterPrime [2..]
  where filterPrime (p:xs) = p : filterPrime [x | x <- xs, x `mod` p /= 0]

loop xs = xs ++ loop xs
```

```console
*Main> :set +s
*Main> take 100 primes
[2,3,5,7,11,13,17,19,23,29,31,37,41,43,47,53,59,61,67,71,73,79,83,89,97,101,103,107,109,113,127,131,137,139,149,151,157,163,167,173,179,181,191,193,197,199,211,223,227,229,233,239,241,251,257,263,269,271,277,281,283,293,307,311,313,317,331,337,347,349,353,359,367,373,379,383,389,397,401,409,419,421,431,433,439,443,449,457,461,463,467,479,487,491,499,503,509,521,523,541]
(0.01 secs, 0 bytes)
```

### strong static typing

No implicit type conversions

```console
Prelude> 3 == 4
False
Prelude> 3 == "true"

<interactive>:4:1:
    No instance for (Num [Char]) arising from the literal ‘3’
    In the first argument of ‘(==)’, namely ‘3’
    In the expression: 3 == "true"
    In an equation for ‘it’: it = 3 == "true"
```

Everything is typed

```console
Prelude> :t "some text"
"some text" :: [Char]
Prelude> :t 'a'
'a' :: Char
Prelude> let f x = x + 1
Prelude> :t f
f :: Num a => a -> a
```

## Other features

### recursion

```haskell
-- examples/example3.hs
factorial n = if n <= 0 then 1 else n * factorial (n - 1)

-- tail recursive version
factorial2 n = fac n 1
  where fac 0 acc = acc
        fac n acc = fac (n - 1) $! acc * n
```

### lists

```haskell
-- examples/example4.hs

-- sugar code version
list1 = [1, 2, 3, 4]

-- it's all head and tail
list2 = 1 : 2 : 3 : 4 : []

-- join lists
list3 = [1, 2] ++ [3, 4]

-- heads off and full reverse
list4 = head list1 : reverse list3

-- nice tail you have there
list5 = tail list4

-- let's take only 10
list6 = take 10 [2,7 ..]

-- let's filter
list7 = filter (>10) [2..20]

list7' = [x | x <- [2..20], x > 10]

-- something a bit less boring
fib = [1, 1] ++ zipWith (+) fib (drop 1 fib)
```

### tuples

```haskell
-- examples/example5.hs

tuple1::(Int, Char)
tuple1 = (1, 's')

tuple2::([Char], Integer)
tuple2 = ("some", 3)

first::[Char]
first = fst tuple2

second::Integer
second = snd tuple2

listOfTuples = zip [1..] [2,4..]
```

### polymorphism and pattern matching

```haskell
-- examples/example6.hs

head'::[t] -> t
head' [] = error "not defined for empty list"
head' (x:xs) = x

snd'::(a, b) -> b
snd' (_, x) = x

map'::(a -> b) -> [a] -> [b]
map' f [] = []
map' f (x:xs) = f x : map' f xs

-- longer form of pattern matching
permutations::[a] -> [[a]]
permutations list =
  case list of
    [] -> [[]]
    (x:xs) -> concatMap (insert x) $ permutations xs
      where
        insert::a->[a]->[[a]]
        insert x [] = [[x]]
        insert x ls@(y:ys) = (x:ls) : (map (y:) $ insert x ys)
```

### lambdas and currying

```haskell
-- examples/example7.hs

func1::Int->Int->Int
func1 x y = x + y * 2

-- is the same as
func2::Int->Int->Int
func2 = \x y -> x + y * 2

-- is the same as
func3::Int->Int->Int
func3 = \x -> \y -> x + y * 2

--that why we can do this
something = func1 2

-- and it's the same as
something2 y = func1 2 y
```

### guards

```haskell
-- examples/example8.hs

someFunction x
  | odd x = "value is odd"
  | x `mod` 3 == 0 = "value is divisible by 3"
  | otherwise = "value is even"
```

### Maybe and Either

```haskell
-- examples/example9.hs

maybeHead::[a] -> Maybe a
maybeHead [] = Nothing
maybeHead (x:_) = Just x

split::[a] -> Maybe (a, [a])
split [] = Nothing
split (x:xs) = Just (x, xs)

multipleFirstTwo::Num a => [a] -> Maybe a
multipleFirstTwo xs = do
  (y, ys) <- split xs
  z <- maybeHead ys
  return $ z * y


fac::(Num a, Ord a, Eq a) => a -> Either [Char] a
fac n
  | n < 0 = Left "Nah..."
  | n == 0 = Right 1
  | otherwise = do
    z <- fac $ n - 1
    return $ z * n
```

### new data types

```haskell
-- examples/example10.hs
import Prelude hiding (Right, Left) -- need to hide Prelude Either imports

data Direction = Up | Down | Right | Left deriving (Show, Eq)

-- Pattern matching works
directionOnMap Up = "North"
directionOnMap Down = "South"
directionOnMap Right = "East"
directionOnMap Left = "West"

-- With some data
data Movement = Move Int | Turn Direction
data Position = Position Int Int Direction deriving (Show)

executeMovement xs = foldl move startPos xs
  where move (Position x y Up) (Move delta) = Position x (y + delta) Up
        move (Position x y Down) (Move delta) = Position x (y - delta) Down
        move (Position x y Right) (Move delta) = Position (x + delta) y Right
        move (Position x y Left) (Move delta) = Position (x - delta) y Left
        move (Position x y _) (Turn direction) = Position x y direction
        startPos = Position 0 0 Up

-- should be Position -12 4 Down
finalPosition = executeMovement [Move 5, Turn Left, Move 10, Move 2, Turn Down, Move 1]

-- With parameterized type
data Tree a b = Node b (Tree a b) (Tree a b) | Leaf a

executeTree::(Tree a b)->(b -> a -> a -> a) -> a
executeTree (Leaf x) _ = x
executeTree (Node key left right) interpret = interpret key valueLeft valueRight
  where valueLeft = executeTree left interpret
        valueRight = executeTree right interpret

data Operator = Plus | Minus

interpretArthmetic::Operator->Int->Int->Int
interpretArthmetic Plus x y = x + y
interpretArthmetic Minus x y = x - y

-- it's just 50 - (20 + 100 - 90)
exampleTree = Node
    Minus
    (Leaf 50)
    $ Node
      Plus
      (Leaf 20)
      $ Node Minus (Leaf 100) (Leaf 90)

executionResult = executeTree exampleTree interpretArthmetic
```

### type classes and Monad (and Applicative functors)

```haskell
-- examples/monad1.hs
data Counter a = Counter Int a deriving (Show)

instance Monad Counter where
  return a = Counter 1 a
  (Counter c a) >>= f = Counter (c+c2) b
      where (Counter c2 b) = f a

-- it's actually defined as function liftM in Control.Monad module
liftToMonad::Monad m => (a -> b) -> m a -> m b
liftToMonad func container = container >>= (\val -> return $ func val)

-- with do notation
liftToMonad'::Monad m => (a -> b) -> m a -> m b
liftToMonad' func container = do
  val <- container
  return $ func val

-- let's get back to trees
-- but this time count how many steps are needed to execute tree

data Tree a b = Node b (Tree a b) (Tree a b) | Leaf a

executeTree::(Tree a b)->(b -> a -> a -> a) -> Counter a
executeTree (Leaf x) _ = return x
executeTree (Node key left right) interpret = do
  leftVal <- executeTree left interpret
  rightVal <- executeTree right interpret
  return $ interpret key leftVal rightVal

data Operator = Plus | Minus

interpretArthmetic::Operator->Int->Int->Int
interpretArthmetic Plus x y = x + y
interpretArthmetic Minus x y = x - y

-- it's just 50 - (20 + 100 - 90)
exampleTree = Node
    Minus
    (Leaf 50)
    $ Node
      Plus
      (Leaf 20)
      $ Node Minus (Leaf 100) (Leaf 90)

executionResult = executeTree exampleTree interpretArthmetic

-- Monad is also Applicative and Functor, so we have to declare those

instance Applicative Counter where
  pure = return
  countedF <*> countedV = do
    func <- countedF
    val <- countedV
    return $ func val

instance Functor Counter where
  fmap func countedVal = countedVal >>= \val -> return $ func val
```
