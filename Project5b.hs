-- import Data.Char (digitToInt)


-- checkSum :: [Int] -> [Int]
-- checkSum  = zipWith (\ x y -> (nextStep x y))  (cycle[1,2]) 
--    where 
--      nextStep x y
--       | y == 1 = x * y
--       | otherwise = div (x*y) 10 + mod (x*y) 10

-- toList :: Int -> [Int]
-- toList = reverse . map digitToInt . show

-- flag :: Int -> Bool
-- flag number = foldl (+) 0 (( checkSum . toList )number )  `mod` 10 == 0 

-- test:: [Bool]
-- test = map flag [49927398716, 49927398717, 1234567812345678, 1234567812345670]
		

-----------------

doubleAndSum :: [Int] -> Int
doubleAndSum  = fst . 
              foldr (\i (acc, even) -> (acc + nextStep even i, not even)) (0, False) 

-- [4,9,9,2] --> 4 -->0,false --> (4,true)
--           --> 9 -->4,true -->
nextStep :: Bool -> Int -> Int
nextStep even i
 | even      = (uncurryFunc (+) . (`divMod` 10) . (*2)) i
 | otherwise = i 

uncurryFunc :: (x->y->z) ->((x,y) -> z)
uncurryFunc f = \(a,b) -> f a b


myLuhn :: Int -> Bool
myLuhn = (0 ==) . (`mod` 10) . doubleAndSum . (map (read . (: ""))) . show 

readInt :: String ->Int
readInt x= read x 

testCC :: [Bool]
testCC = map myLuhn [49927398716, 49927398717, 1234567812345678, 1234567812345670]

t :: [Char] -> [Int]
t = (map (read . (: ""))) 

eg :: Integer -> Integer -> Integer
eg  = div 

-- tt :: [Char] -> [Int]
tt = (: "") 


