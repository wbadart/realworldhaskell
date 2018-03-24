import Data.Function (on)
import Data.List (genericLength, intersperse, minimumBy, sortBy)
import Test.QuickCheck (quickCheck)


-- Exercise 1, 2
myLength :: [a] -> Int
myLength = len 0
    where len n []     = n
          len n (_:xs) = len (n + 1) xs

myLength' :: [a] -> Int
myLength' = foldl (\n _ -> n + 1) 0


-- Exercise 3
mean :: Integral a => [a] -> Double
mean xs = fromIntegral (sum xs) / genericLength xs


-- Exercise 4, 5
pal :: [a] -> [a]
pal xs = xs ++ reverse xs

ispal :: Eq a => [a] -> Bool
ispal xs = take split xs == reverse (drop split xs)
    where split = length xs `div` 2


-- Exercise 6
sortOnSubLen :: [[a]] -> [[a]]
sortOnSubLen = sortBy (compare `on` length)


-- Exercise 7
myIntersperse :: a -> [a] -> [a]
myIntersperse sep []     = []
myIntersperse sep [x]    = [x]
myIntersperse sep (x:xs) = x : (sep : myIntersperse sep xs)


-- Exercise 8
data Tree a = Node a (Tree a) (Tree a)
            | Empty
            deriving (Show)

height :: Tree a -> Int
height Empty = 0
height (Node _ a b) = 1 + max (height a) (height b)


-- Exercise 9, 10, 11
data Direction = L | R | Straight deriving (Eq, Show)
data Point     = Point {x :: Double, y :: Double} deriving (Show)

direction :: Point -> Point -> Point -> Direction
direction p1 p2 p3 =
    case compare (cross p1 p2 p3) 0 of
        GT -> L
        LT -> R
        EQ -> Straight
    where cross (Point x1 y1) (Point x2 y2) (Point x3 y3) =
            (x2 - x1) * (y3 - y1) - (y2 - y1) * (x3 - x1)

turns :: [Point] -> [Direction]
turns [p1, p2, p3] = [direction p1 p2 p3]
turns (p1:p2:p3:ps) = direction p1 p2 p3 : turns (p2:p3:ps)


-- Exercise 12
graham :: [Point] -> [Point]
graham ps =
    let p      = minimumBy (compare `on` y) ps
        sorted = sortBy (compare `on` dot p) ps
    in  reverse $ foldl lefts [] sorted
  where
    dot (Point x1 y1) (Point x2 y2) = (x1 * x2) + (y1 * y2)
    lefts [] p         = [p]
    lefts [p1] p2      = [p2, p1]
    lefts (p2:p1:ps) p3 = if direction p1 p2 p3 /= L
                            then [p3, p1] ++ ps else [p3, p2, p1] ++ ps

a = Point 0 0
b = Point 1 1
c = Point 2 2
d = Point 3 4
e = Point 3 2

pts = [Point 0 0, Point 1 0, Point 2 1, Point 1 2, Point 1 3, Point 0 1]

checkLength xs          = myLength xs == length xs
checkLength' xs         = myLength' xs == length xs
checkPal xs             = ispal (pal xs)
checkIntersperse sep xs = myIntersperse sep xs == intersperse sep xs

main = do
    putStrLn "Checking myLength..."
    quickCheck (checkLength :: [Int] -> Bool)

    putStrLn "\nChecking myLength'..."
    quickCheck (checkLength' :: [Int] -> Bool)

    putStrLn "\nChecking pal/ ispal..."
    quickCheck (checkPal :: String -> Bool)

    putStrLn "\nChecking myIntersperse..."
    quickCheck (checkIntersperse :: Char -> String -> Bool)

    return ()
