import Data.Char (digitToInt)

digitSum :: Int -> Int
digitSum num = (div num 10) + (mod num 10)
 -- | num < 10 = num
 -- | otherwise = num - 9

checkSum :: [Int] -> [Int]
checkSum num = map digitSum (zipWith (*) num (cycle[1,2]) )

toIntList :: [Char] -> [Int]
toIntList number = reverse(map digitToInt number)

flag :: [Char] -> Bool
flag number = myFoldl (+) 0 (( checkSum . toIntList )number ) `mod` 10 == 0  -- calculateDigitSum .


myFoldl :: (b -> a -> b) -> b -> [a] -> b
myFoldl = foldl

-- foldl (++) "a" ["qwer","fdas","dsaf"]
-- [1a,2a,3a,4a,5a]


-- calculateDigitSum :: [Int] -> [Int]
-- calculateDigitSum num = map digitSum num

--Valid Card Numbers
-- 4429916945070896
-- 36594659453682

