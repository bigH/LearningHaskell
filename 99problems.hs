module H99 where

import Data.List
import Data.Function

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
myGroup :: Eq a => [Integer] -> [a] -> [[[a]]]
myGroup [] _ = []
myGroup _ [] = []
myGroup (_:[]) xs = [[xs]]
myGroup (l:ls) xs = [ fst combos : otherCombos |
                    combos <- combinationTuples l xs,
                    otherCombos <- myGroup ls (snd combos) ]

-- 28
lsort :: [[a]] -> [[a]]
lsort [] = []
lsort (x:xs) = smaller ++ [x] ++ larger
  where smaller = lsort [ y | y <- xs, myLength y <= myLength x ]
        larger = lsort [ y | y <- xs, myLength y > myLength x ]

reduce :: ([a] -> [a] -> [a]) -> [[a]] -> [a]
reduce f = foldl f []

lfsort :: [[a]] -> [[a]]
lfsort = reduce (++) . lsort . groupBy ((==) `on` myLength) . lsort

-- 31
isPrime :: Integer -> Bool
isPrime 1 = False
isPrime 2 = True
isPrime x
  | x <= 0    = error "Positive Only"
  | otherwise = all ((/= 0) . (x `mod`)) [2 .. x-1]

-- 32
myGCD :: Integer -> Integer -> Integer
myGCD x y = product $ factorsX `intersect` factorsY
            where primeFactors' j = [i | i <- [1..j], isPrime i, j `mod` i == 0]
                  factorsX = primeFactors' x
                  factorsY = primeFactors' y

-- 33
coprime :: Integer -> Integer -> Bool
coprime x y = 1 == myGCD x y

-- 34
toitent :: Integer -> Integer
toitent 1 = 1
toitent m = sum [1 | x <- [1 .. m-1], coprime m x ]

-- 35
primeFactors :: Integer -> [Integer]
primeFactors 1 = []
primeFactors x
  | isPrime x = [x]
  | otherwise = firstFactor : primeFactors (x `quot` firstFactor) 
  where firstFactor = head [ y | y <- [2 .. x-1], isPrime y, x `mod` y == 0 ]

-- 36
primeFactorsEncoded :: Integer -> [(Integer, Integer)]
primeFactorsEncoded = encode . primeFactors

-- 37
toitent' :: Integer -> Integer
toitent' = product . map (\ (m, p) -> (p - 1) * p ^ (m - 1)) . primeFactorsEncoded

-- 39
primesR :: Integer -> Integer -> [Integer]
primesR x y = [i | i <- [x .. y], isPrime i]

-- 40
goldbach :: Integer -> (Integer, Integer)
goldbach x
  | x `mod` 2 == 1 = error "Only even numbers work."
  | otherwise      = findPairThatSumsTo x (primesR 2 x)
    where findPairThatSumsTo :: Integer -> [Integer] -> (Integer, Integer)
          findPairThatSumsTo _ [] = error "Did not find matching pair."
          findPairThatSumsTo s ps@(p:prest)
            | (s - p) `elem` ps = (p, s - p)
            | otherwise         = findPairThatSumsTo s prest

-- 41
goldbachRange :: Integer -> Integer -> [(Integer, Integer)]
goldbachRange x y = [ goldbach i | i <- [x .. y], i `mod` 2 == 0 ]

and' :: Bool -> Bool -> Bool
and' True True = True
and' _ _ = False

equ' :: Bool -> Bool -> Bool
equ' True True = True
equ' False False = True
equ' _ _ = False

or' :: Bool -> Bool -> Bool
or' True _ = True
or' _ True = True
or' _ _ = False

-- 46
table :: (Bool -> Bool -> Bool) -> [[Bool]]
table f = [ [x, y, f x y] | x <- [True, False], y <- [True, False] ]

perms :: [a] -> Integer -> [[a]]
perms _ 0 = [[]]
perms choices i = [ choice : perm | choice <- choices, perm <- perms choices (i-1) ]

-- 48
tablen :: Integer -> ([Bool] -> Bool) -> [[Bool]]
tablen 0 _ = []
tablen c f = [ permutation ++ [f permutation] | permutation <- perms [True, False] c ]

-- 49
gray :: Integer -> [String]
gray 1 = ["0", "1"]
gray n = let other = gray (n - 1)
             len = myLength other
             otherDoubled = other ++ myReverse other
             grayCloned = repli (gray 1) len
         in zipWith (++) grayCloned otherDoubled


data Tree a = Empty | Branch a (Tree a) (Tree a)
              deriving (Show, Eq)

instance Functor Tree where 
  fmap _ Empty = Empty
  fmap f (Branch x left right) = Branch (f x) (fmap f left) (fmap f right)

-- 50


-- 55
completeBalance :: a -> Integer -> [Tree a]
completeBalance _ 0 = [Empty]
completeBalance a n =
  let (q, r) = (n - 1) `quotRem` 2
  in [Branch a left right | l <- [q .. q + r],
                            left <- completeBalance a l,
                            right <- completeBalance a (n - 1 - l) ]

-- 56
-- mirror helper
mirror :: Tree a -> Tree a -> Bool
mirror Empty Empty = True
mirror (Branch _ ll lr) (Branch _ rl rr) = mirror ll rl && mirror lr rr
mirror _ _ = False

symmetric :: Tree a -> Bool
symmetric Empty = True
symmetric (Branch _ l r) = mirror l r

-- 57
buildBTree ::  Ord a => [a] -> Tree a
buildBTree [] = Empty
buildBTree (x:rest) = Branch x (buildBTree left) (buildBTree right)
                      where left = [i | i <- rest, i <= x]
                            right = [i | i <- rest, i > x]

-- 58
balancedSymmetric :: a -> Integer -> [Tree a]
balancedSymmetric a i = filter symmetric $ completeBalance a i

-- 59
heightBalance :: a -> Integer -> [Tree a]
heightBalance _ 0 = [Empty]
heightBalance a 1 = [Branch a Empty Empty]
heightBalance a n = [Branch a left right
                    | (leftHeight, rightHeight) <- [(n - 1, n - 1), (n - 2, n - 1), (n - 1, n - 2)]
                    , left <- heightBalance a leftHeight
                    , right <- heightBalance a rightHeight ]

-- treeSize
treeSize :: Tree a -> Integer
treeSize Empty = 0
treeSize (Branch _ left right) = 1 + treeSize left + treeSize right

-- quickSort
quickSort :: Ord a => [a] -> [a]
quickSort [] = []
quickSort (a:as) = left ++ [a] ++ right
  where left = quickSort [x | x <- as, x <= a]
        right = quickSort [x | x <- as, x > a]

-- 60
nodeCountBalanced :: a -> Integer -> [Tree a]
nodeCountBalanced _ 0 = [Empty]
nodeCountBalanced a 1 = [Branch a Empty Empty]

expectedHeight :: Integer -> Integer
expectedHeight 0 = 0

