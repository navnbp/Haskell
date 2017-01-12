import Data.List 
import Data.Maybe

data N_ary = N_ary String Int

digits :: [Char]
digits = ['0' .. '9'] ++ ['A' .. 'Z'] ++ ['a' .. 'z']

digitToInt :: Char -> Integer 
digitToInt ch 
    | ch `elem` ['a'..'z'] = fromIntegral(fromJust(elemIndex ch ['a'..'z']) + 10)
    | ch `elem` digits = fromIntegral(fromJust(elemIndex ch digits))
    | otherwise  = 0::Integer
-- Hint: Use the function elemIndex.

valueOfString :: N_ary -> String  
valueOfString (N_ary valueOfString _) = valueOfString

getBase :: N_ary -> Int  
getBase (N_ary _ getBase) = getBase

(~~) :: String -> Int -> N_ary
(~~) a b= N_ary a b


instance Show N_ary where
	show a = valueOfString a ++ getBase a