-- examples/example7.hs

func1::Int->Int->Int
func1 x y = x + y * 2

-- is the same as
func2::Int->Int->Int
func2 = \x y -> x + y * 2

-- is the same as
func3::Int->Int->Int
func3 = \x -> \y -> x + y * 2

--that why we can do this
something = func1 2

-- and it's the same as
something2 y = func1 2 y
