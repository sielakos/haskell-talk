-- examples/example3.hs
factorial n = if n <= 0 then 1 else n * factorial (n - 1)
-- it's actually quite bad implementation

-- tail recursive version
factorial2 n = fac n 1
  where fac 0 acc = acc
        fac n acc = fac (n - 1) $! acc * n 
