{-
  1.  Given n, write a function that returns a list of the first n positive squares 
  > allsquares 5
  [1,4,9,16,25]
-}
allsquares n = [n * n | n <- [1..n]]


{-
  2.  Write a sublist function that returns a portion of a list given the first and last (one-based) index
  > sublist 1 3 [1,4,9,6,5]
  [1,4,9]
  > sublist 3 6 [1,4,9,6,5,2]
  [9,6,5,2]
-}
sublist x y lst = take (y + 1 - x) (drop (x - 1) lst)


{-
  3.  Write a merge function to support mergesort; it should be able to merge two already-sorted lists into a single sorted list.
  > merge [1,3,5] [2,6,7,8]
  [1,2,3,5,6,7,8]
-}
merge [] lst = lst
merge lst [] = lst
merge (x:xs) (y:ys) = 
	if x <= y
		then x : merge xs (y:ys)
		else y : merge (x:xs) ys


{-
  4.  Write a mergesort function based on the merge function above.
  > mergesort [1,4,9,6,5]
  [1,4,5,6,9]
-}
mergesort [] = []
mergesort [x] = [x]
mergesort lst = merge (mergesort l1) (mergesort l2)
    where
	l1 = take len lst
	l2 = drop len lst
	len = length lst `div` 2

	
	