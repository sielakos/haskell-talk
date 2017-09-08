-- examples/example8.hs

someFunction x
  | odd x = "value is odd"
  | x `mod` 3 == 0 = "value is divisible by 3"
  | otherwise = "value is even"
