-- examples/example6.hs

head'::[t] -> t
head' [] = error "not defined for empty list"
head' (x:xs) = x

snd'::(a, b) -> b
snd' (_, x) = x

map'::(a -> b) -> [a] -> [b]
map' f [] = []
map' f (x:xs) = f x : map' f xs

-- longer form of pattern matching
permutations::[a] -> [[a]]
permutations list =
  case list of
    [] -> [[]]
    (x:xs) -> concatMap (insert x) $ permutations xs
      where
        insert::a->[a]->[[a]]
        insert x [] = [[x]]
        insert x ls@(y:ys) = (x:ls) : (map (y:) $ insert x ys)
