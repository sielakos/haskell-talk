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

*non-strict semantics* - lazy execution, nothing is evaluated until needed and it's done only once.
