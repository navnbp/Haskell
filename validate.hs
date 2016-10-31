import Data.Char (digitToInt)

--toList :: [String] -> [Int]
-- map digitToInt number



weightPattern = [1,2,1,2,1,2,1,2,1,2,1,2,1,2,1,2]

--weightSum  = zipWith (*) [1,2] [2,4] 

-- (map digitToInt number) weightPattern

validate :: String -> String
validate number
 | length number == 4 = "Valid Card"
 | otherwise =  "Invalid Card"



