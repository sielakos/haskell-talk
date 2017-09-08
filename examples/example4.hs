-- examples/example4.hs

-- sugar code version
list1 = [1, 2, 3, 4]

-- it's all head and tail
list2 = 1 : 2 : 3 : 4 : []

-- join lists
list3 = [1, 2] ++ [3, 4]

-- heads off and full reverse
list4 = head list1 : reverse list3

-- nice tail you have there
list5 = tail list4

-- let's take only 10
list6 = take 10 [2,7 ..]

-- let's filter
list7 = filter (>10) [2..20]

list7' = [x | x <- [2..20], x > 10]

-- something a bit less boring
fib = [1, 1] ++ zipWith (+) fib (drop 1 fib)
