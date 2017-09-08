-- examples/example5.hs

tuple1::(Int, Char)
tuple1 = (1, 's')

tuple2::([Char], Integer)
tuple2 = ("some", 3)

first::[Char]
first = fst tuple2

second::Integer
second = snd tuple2

listOfTuples = zip [1..] [2,4..]
