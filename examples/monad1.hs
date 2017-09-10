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
