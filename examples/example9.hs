-- examples/example9.hs

maybeHead::[a] -> Maybe a
maybeHead [] = Nothing
maybeHead (x:_) = Just x

split::[a] -> Maybe (a, [a])
split [] = Nothing
split (x:xs) = Just (x, xs)

multipleFirstTwo::Num a => [a] -> Maybe a
multipleFirstTwo xs = do
  (y, ys) <- split xs
  z <- maybeHead ys
  return $ z * y


fac::(Num a, Ord a, Eq a) => a -> Either [Char] a
fac n
  | n < 0 = Left "Nah..."
  | n == 0 = Right 1
  | otherwise = do
    z <- fac $ n - 1
    return $ z * n
