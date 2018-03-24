import Data.Char (digitToInt, isDigit)
import Data.List (groupBy, transpose)
import Test.QuickCheck (quickCheck)

-- Exercise 1

safeHead :: [a] -> Maybe a
safeHead []    = Nothing
safeHead (x:_) = Just x

safeTail :: [a] -> Maybe [a]
safeTail []     = Nothing
safeTail (_:xs) = Just xs

safeLast :: [a] -> Maybe a
safeLast []     = Nothing
safeLast [x]    = Just x
safeLast (_:xs) = safeLast xs

safeInit :: [a] -> Maybe [a]
safeInit []     = Nothing
safeInit [x]    = Just []
safeInit (x:xs) = Just (x : inner xs)
  where inner [x]    = []
        inner (x:xs) = x : inner xs

-- OR:
mksafe :: ([a] -> b) -> [a] -> Maybe b
mksafe f [] = Nothing
mksafe f xs = Just (f xs)

safeHead' = mksafe head
safeTail' = mksafe tail
safeLast' = mksafe last
safeInit' = mksafe init


-- Exercise 2
splitWith :: (a -> Bool) -> [a] -> [[a]]
splitWith p = foldr mkfolds [[]]
  where mkfolds x acc@(ys:yss)
            | p x       = (x : ys) : tail acc
            | otherwise = [] : acc
            -- | otherwise = [x] : acc


-- Exercise 3
-- main = interact $ unlines .firstWords
firstWords = map (head . words) . lines


-- Exercise 4
-- main = interact $ unlines . myTranspose . lines
myTranspose :: [[a]] -> [[a]]
myTranspose xs | null xs     = []
               | any null xs = []
               | otherwise   = map head xs : myTranspose (map tail xs)


-- Part 2
-- Exercise 1, 2
asInt :: String -> Int
asInt ('-':xs) = negate $ asInt xs
asInt xs       = foldl mkint 0 xs
  where mkint acc x = acc * 10 + digitToInt x

safeAsInt :: String -> Maybe Int
safeAsInt ('-':xs) = negateMaybe $ safeAsInt xs
  where negateMaybe (Just x) = Just (-x)
        negateMaybe Nothing  = Nothing
safeAsInt xs | any (not . isDigit) xs = Nothing
             | otherwise              = Just (asInt xs)


-- Exercise 3
myConcat :: [[a]] -> [a]
myConcat = foldr flatten []
  where flatten xs acc = xs ++ acc
-- myConcat = foldr (++) []


-- Exercise 4
myTakeWhile :: (a -> Bool) -> [a] -> [a]
myTakeWhile p [] = []
myTakeWhile p xs = inner p xs []
  where inner p (x:xs) prev | not (p x) = prev
                            | otherwise = inner p xs (x : prev)

myTakeWhile' :: (a -> Bool) -> [a] -> [a]
myTakeWhile' p = foldr findBreak []
  where findBreak x acc | p x       = x : acc
                        | otherwise = []


-- Exercise 5
myGroupBy :: (a -> a -> Bool) -> [a] -> [[a]]
myGroupBy f = foldr mkgroups []
    where mkgroups x [] = [[x]]
          mkgroups x groups@(group:other)
            | f x (head group) = (x : group) : other
            | otherwise        = [x] : groups


-- Exercise 6
myAny :: (a -> Bool) -> [a] -> Bool
myAny p = foldr (||) False . map p

myCycle :: [a] -> [a]
myCycle xs = foldr loop xs [1..]
  where loop _ acc = xs ++ acc

myWords :: String -> [String]
myWords "" = []
myWords s  = foldr mkwords [] s
  where mkwords c [] = [[c]]
        mkwords c all@(wd:wds) | c /= ' '  = (c : wd) : wds
                               | otherwise = "" : all
