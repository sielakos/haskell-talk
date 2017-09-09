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
