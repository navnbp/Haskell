import Data.List
import Data.Char (digitToInt)

toDigits :: Int -> [Int]
toDigits = map digitToInt . show

-- myCompare :: Int -> Int -> Ordering
-- myCompare x y
--  | x1 == y1 && x < 10 = flip compare x (getSecondDigitOn y)          --53 534 
--  | x1 == y1 && y < 10 = flip compare (getSecondDigitOn x) y   
--  | x1 == y1 = myCompare (getSecondDigitOn x) (getSecondDigitOn y)
--  | otherwise = flip compare x1 y1
--    where x1 = head (toDigits x)
--          y1 = head (toDigits y)

concatNum :: Int -> Int -> Int
concatNum x y = read ((show x) ++ (show y))

myCompare1 :: Int -> Int -> Ordering
myCompare1 x y = flip compare (concatNum x y) (concatNum y x)

-- | x < 10 && y < 10 = flip compare x y
--  | x < 10 = flip compare x y1
--  | y < 10 = flip compare x1 y

getSecondDigitOn :: Int -> Int
getSecondDigitOn x  
 | num == "" = 0
 | otherwise = read num
   where num = tail (show x)

-- 1234 -> 234

largestNumber :: [Int] -> Integer
largestNumber =  read . concat . map show . sortBy myCompare1 

mydot ::  (b -> c) -> (a -> b) -> (a -> c)
mydot f g = \x -> f (g x)

test :: [Int] -> [Int]
test = sortBy myCompare 


-- test  [44,445,545,55,33,5,4,59]