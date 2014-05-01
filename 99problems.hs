module H99 where

-- 1
myLast :: [a] -> a
myLast [] = error "Nothing in list"
myLast (x:[]) = x
myLast (_:xs) = myLast xs

-- 2
myButLast :: [a] -> a
myButLast [] = error "List must be > 1 in length"
myButLast (_:[]) = error "List must be > 1 in length"
myButLast (x:_:[]) = x
myButLast (_:xs) = myButLast xs

-- 3
elementAt :: [a] -> Integer -> a
elementAt (x:_) 0 = x
elementAt (_:xs) i = elementAt xs (i - 1)

-- 4
myLength :: [a] -> Integer
myLength [] = 0
myLength (_:xs) = myLength xs + 1

-- 5
myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = myReverse xs ++ [x]

-- 6
isPalindrome :: Eq a => [a] -> Bool
isPalindrome xs = myReverse xs == xs

-- 7
data NestedList a = Elem a | List [NestedList a]
flatten :: NestedList a -> [a]
flatten (Elem x) = [x]
flatten (List []) = []
flatten (List (x:xs)) = flatten x ++ flatten (List xs)

-- 8
compress :: Eq a => [a] -> [a]
compress [] = []
compress (x:[]) = [x]
compress (x:y:rest)
  | x == y    = compress (x:rest)
  | otherwise = x:compress (y:rest)

-- 9
pack :: Eq a => [a] -> [[a]]
pack [] = []
pack (x:[]) = [[x]]
pack (x:y:rest)
          | x == y    = (x : head packed) : tail packed
          | otherwise = [x] : packed
      where packed = pack (y:rest)

-- 10
encode :: Eq a => [a] -> [(Integer, a)]
encode [] = []
encode (x:[]) = [(1, x)]
encode (x:y:rest)
          | x == y    = x `join` head encoded : tail encoded
          | otherwise = (1, x) : encoded
      where encoded = encode (y:rest)
            join _ (count, a) = (count + 1, a)

-- helper
myRepeat :: a -> Integer -> [a]
myRepeat _ 0 = []
myRepeat x c = x : myRepeat x (c - 1)

-- 10 (alt)
decode :: [(Integer, a)] -> [a]
decode [] = []
decode ((count, a):rest) = myRepeat a count ++ decode rest

-- 11
data Encoding a = Single a | Multiple Integer a
encodeModified :: Eq a => [a] -> [Encoding a]
encodeModified [] = []
encodeModified (x:[]) = [Single x]
encodeModified (x:y:rest)
          | x == y    = x `join` head encoded : tail encoded
          | otherwise = Single x : encoded
      where encoded = encodeModified (y:rest)
            join _ (Single a) = Multiple 2 a
            join _ (Multiple count a) = Multiple (count + 1) a

-- 12
decodeModified :: [Encoding a] -> [a]
decodeModified [] = []
decodeModified (Single a:rest) = a : decodeModified rest
decodeModified (Multiple count a:rest) = myRepeat a count ++ decodeModified rest

-- 13 -- skip

-- 14
dupli :: [a] -> [a]
dupli [] = []
dupli (x:rest) = x:x:dupli rest

-- 15
repli :: [a] -> Integer -> [a]
repli [] _ = []
repli (x:rest) count = myRepeat x count ++ repli rest count

-- 16
dropEvery :: [a] -> Integer -> [a]
dropEvery l c = dropEvery' l c c
  where dropEvery' :: [a] -> Integer -> Integer -> [a]
        dropEvery' [] _ _ = []
        dropEvery' (_:xs) t 1 = dropEvery' xs t t
        dropEvery' (x:xs) t i = x : dropEvery' xs t (i - 1)

myDrop :: [a] -> Integer -> [a]
myDrop [] _ = []
myDrop xs 0 = xs
myDrop (_:xs) i = myDrop xs (i-1)

myTake :: [a] -> Integer -> [a]
myTake [] _ = []
myTake _ 0 = []
myTake (x:xs) i = x : myTake xs (i-1)

-- 17
split :: [a] -> Integer -> ([a], [a])
split xs i = (myTake xs i, myDrop xs i)

-- 18
slice :: [a] -> Integer -> Integer -> [a]
slice xs i j = myDrop (myTake xs j) (i-1)

-- 19
rotate :: [a] -> Integer -> [a]
rotate [] _ = []
rotate xs 0 = xs
rotate xs i = myDrop xs spot ++ myTake xs spot 
              where spot = i `mod` myLength xs

-- 20
removeAt :: [a] -> Integer -> [a]
removeAt xs i = myTake xs (i - 1) ++ myDrop xs i

-- 21
insertAt :: [a] -> a -> Integer -> [a]
insertAt xs x i = myTake xs i ++ [x] ++ myDrop xs i

-- 22
range :: Enum a => Eq a => a -> a -> [a]
range x y
  | x == y = [x]
  | otherwise = x : range (succ x) y

-- 23 -- skipped because IO
-- 24 -- skipped because IO
-- 25 -- skipped because IO

-- 26
-- WRONG PROBLEM ..
-- combinations :: Integer -> [a] -> [[a]]
-- combinations 0 _ = []
-- combinations 1 xs = [ [x] | x <- xs ]
-- combinations n xs = [ x:others | x <- xs, others <- combinations (n-1) xs ]
combinations :: Integer -> [a] -> [[a]]
combinations 0 _ = []
combinations _ [] = []
combinations 1 xs = [ [x] | x <- xs ]
combinations n (x:xs) = [ x:furtherCombination | furtherCombination <- combinations (n-1) xs ] ++ combinations n xs

elementIn :: Eq a => [a] -> a -> Bool
elementIn [] _ = False
elementIn (x:xs) y
  | x == y    = True
  | otherwise = elementIn xs y

combinationTuples :: Eq a => Integer -> [a] -> [([a], [a])]
combinationTuples i xs = [ (combo, [ x | x <- xs, x `notElem` combo ]) |
                           combo <- combinations i xs ]

-- 27
group :: Eq a => [Integer] -> [a] -> [[[a]]]
group [] _ = []
group _ [] = []
group (_:[]) xs = [[xs]]
group (l:ls) xs = [ fst combos : otherCombos |
                    combos <- combinationTuples l xs,
                    otherCombos <- group ls (snd combos) ]

-- 28
lsort :: [[a]] -> [[a]]
lsort [] = []
lsort (x:xs) = smaller ++ [x] ++ larger
  where smaller = lsort [ y | y <- xs, myLength y <= myLength x ]
        larger = lsort [ y | y <- xs, myLength y >= myLength x ]


