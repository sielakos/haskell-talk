-- examples/program1.hs
import System.Environment
import Data.List
import Control.Monad

main = do
  args <- getArgs
  -- mapM_ is like map, but for also accumulates monads
  mapM_ (\arg -> print $ sum [1 .. read arg]) args
