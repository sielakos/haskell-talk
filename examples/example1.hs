-- examples/example1.hs
addOne::Int -> Int
addOne x = x + 1

hypotenuse::Floating a => a -> a -> a
hypotenuse a b = (a**2 + b**2)**(1.0/2)

hypo3::Floating a => a -> a
hypo3 = hypotenuse 3

applyToList f = map f [1..10]
