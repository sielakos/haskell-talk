-- examples/example2.hs
primes = filterPrime [2..]
  where filterPrime (p:xs) = p : filterPrime [x | x <- xs, x `mod` p /= 0]

loop xs = xs ++ loop xs
