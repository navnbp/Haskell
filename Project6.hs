import Data.List
import Data.Ord 


order :: String -> String -> Ordering
order x y 
 | x == [] || y == [] =  compare x y  -- compare 5 54
 | x < "10" && y < "10" =  myflip compare x y  
 | x1 == y1  = order xs ys                   -- 553 554
 | otherwise = flip compare x1 y1
    where x1:xs = x
          y1:ys = y

--["5","6"]["55,554"] 

myflip ::  (a -> b -> c) -> (b -> a -> c)
myflip f x y= f y x

largestNumber  :: [Integer] -> Integer
largestNumber  x =   read . concat .  sortBy order $ map show x
-- largestNumber  [44,445,545,55,33,5,4,59]

test :: [Integer] -> [String]
test x = sortBy order $ map show x
-- test  [44,445,545,55,33,5,4,59]

mydot ::  (b -> c) -> (a -> b) -> (a -> c)
mydot f g = \x -> f (g x)

-- readInt :: String -> Integer
-- readInt x = read x

