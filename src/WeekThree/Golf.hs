module WeekThree.Golf where

-- Exercise 1 - Hopscotch
-- ------------------------
-- The first list in the output should
-- be the same as the input list. The second list in the output should
-- contain every second element from the input list. . . and the nth list in
-- the output should contain every nth element from the input list.
-- For example:
-- skips "ABCD" == ["ABCD", "BD", "C", "D"]
-- skips "hello!" == ["hello!", "el!", "l!", "l", "o", "!"]
-- skips [1] == [[1]]
-- skips [True,False] == [[True,False], [False]]
-- skips [] == []
--
--
-- returns every nth element from a list. We use drop to remove the first n - 1 items from the
-- list and take the first element of the new list which is our first nth element. Then, we
-- repeat the process with the remainder of the new list.
everyNth :: Int -> [a] -> [a]
everyNth _ [] = []
everyNth n list =
  case drop (n - 1) list of
    [] -> []
    x:xs -> x : everyNth n xs

-- Using list comprehension, for 1 until the length of the list we call everyNth with this
-- increasing variable passing the list each time. Once we have called everyNth with n equal
-- to the length of the list, we are done. This ensures that the length of the resulting list
-- is equal to the initial list.
skips :: [a] -> [[a]]
skips [] = []
skips list = [everyNth i list | i <- [1 .. length list]]

--
-- Exercise 2 - Local Maxima
-- ------------------------
-- finds all the local maxima in the input list and returns them in
-- order. A local maximum of a list is an element of the list which is strictly
-- greater than both the elements immediately before and after it.
-- For example:
-- localMaxima [2,9,5,6,1] == [9,6]
-- localMaxima [2,3,4,1,5] == [4]
-- localMaxima [1,2,3,4,5] == []
isLocalMaximum :: (Integer, Integer, Integer) -> Bool
isLocalMaximum (a, b, c) = b > c && b > a

localMaxima :: [Integer] -> [Integer]
localMaxima [] = []
localMaxima [_] = []
localMaxima [_, _] = []
localMaxima (x1:x2:x3:xs) =
  [x2 | isLocalMaximum (x1, x2, x3)] ++ localMaxima (x2 : x3 : xs)
