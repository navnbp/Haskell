import Data.List
import Data.Ord 


concatNum :: Int -> Int -> Int
concatNum x y = read ((show x) ++ (show y))

myCompare :: Int -> Int -> Ordering
myCompare x y = flip compare (concatNum x y) (concatNum y x)

largestNumber :: [Int] -> Integer
largestNumber =  read . concat . map show . sortBy myCompare 


mydot ::  (b -> c) -> (a -> b) -> (a -> c)
mydot f g = \x -> f (g x)


myflip ::  (a -> b -> c) -> (b -> a -> c)
myflip f x y= f y x

-- [34 35] --> [35,34]