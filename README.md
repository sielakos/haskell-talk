# Quick introduction to Haskell

## History

Named after Haskell Curry and rest is too boring, so let's skip it.

## How to get it?

https://www.haskell.org/

## What the hell is that Haskell anyway?

So according to Wiki:

Haskell is a standardized, general-purpose purely functional programming language, with non-strict semantics and strong static typing.

*But what the hell does terms mean anyway?*

So...

*purely functional* - mathematical functions, immutable data and no side effects.

```haskell
addOne::Int -> Int
addOne x = x + 1

hypotenuse:: Floating a => a -> a -> a
hypotenuse a b = (a**2 + b**2)**(1.0/2)

hypo3::Floating a => a -> a
hypo3 = hypotenuse 3
```

```console
mariusz:~/playground/haskell-talk (master)$ ghci
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
```


*non-strict semantics* - lazy execution, nothing is evaluated until needed and it's done only once.
