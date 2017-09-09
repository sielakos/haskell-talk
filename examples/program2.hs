-- examples/program2.hs
import Data.List
import Control.Monad

main = do
  mapM_ (print.odd) [value, value]

-- Interesingly enough using call to factorial 200000 twice instead of value
-- result in twice computation effort. 
value = factorial 200000

factorial::Integer -> Integer
factorial n
  | n == 0 = 1
  | n > 0  = n * (factorial $ n - 1)
