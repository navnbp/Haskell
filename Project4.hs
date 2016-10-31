import Data.Char (digitToInt)

--digitSum :: Integer -> Integer
--digitSum no = divisor + remainder where (divisor,remainder) = (div no 10, mod no 10)

digitSum :: Integer -> Integer
digitSum num
 | num < 10 = num
 | otherwise = num - 9

checkSum [] = 0
checkSum [x] = x
checkSum (x:y:xs) =   digitSum ( y * 2 ) + x + checkSum xs 


convertAndReverse :: Integer -> [Integer]
convertAndReverse 0 = []
convertAndReverse num = remainder : convertAndReverse (divisor)
 where (divisor,remainder) = (div num 10, mod num 10)

-- toList :: [Char] -> [Int]
-- toList number = reverse(map digitToInt number)

toInt :: String -> Integer
toInt num = read num :: Integer


flag :: String -> Bool
flag number = (( checkSum . convertAndReverse . toInt) number)  `mod` 10 == 0


isValidate :: String -> String
isValidate number
 | flag number == True = "Valid Card"
 | otherwise = "Invalid Card"

--Valid Card Numbers
-- 4429916945070896
-- 36594659453682



