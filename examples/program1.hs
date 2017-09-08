-- examples/program1.hs
import System.Environment
import Data.List

main = do
  args <- getArgs
  foldl1 (\y x -> y >> x) $ map (\arg -> print $ show $ sum [1 .. read arg]) args
