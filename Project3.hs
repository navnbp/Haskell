getValue :: Char ->  Int 
getValue c
 | c == 'M' = 1000
 | c == 'D' = 500
 | c == 'C' = 100
 | c == 'L' = 50
 | c == 'X' = 10
 | c == 'V' = 5
 | c == 'I' = 1
 | otherwise = 0 
-- where c = head(roman)

validCombination :: String -> Bool
validCombination('C':'M':_) = True
validCombination('C':'D':_) = True
validCombination('X':'C':_) = True
validCombination('X':'L':_) = True
validCombination('I':'M':_) = True
validCombination('I':'V':_) = True
validCombination(_) = False


toArabic :: String -> Int
toArabic "" = 0
toArabic str = num + toArabic rest
    where (num, rest) = calculate str

calculateValue roman =  if length roman > 1
					      then if (getValue(head roman) < getValue(head (tail roman)) && validCombination roman == True)
					             then (getValue(head (tail roman)) - getValue(head roman), tail (tail roman))
					           else (getValue(head roman),tail roman)
				        else (getValue(head roman), "")


calculate [y] = (getValue(y), "")
calculate (y:z:xs)  
                    | getValue(y) < getValue(z) = (getValue(z)-getValue(y), xs)
                    | otherwise = (getValue(y), (z:xs))
--

oneStep :: String  -> (Int, String)
oneStep ('M' : rest) = (1000, rest)
oneStep ('C' : 'M' : rest) = (900, rest)
oneStep ('D' : rest) = (500, rest)
oneStep ('C' : 'D' : rest) = (400, rest)
oneStep ('C' : rest) = (100, rest)
oneStep ('X' : 'C' : rest) = (90, rest)
oneStep ('L' : rest) = (50, rest)
oneStep ('X' : 'L' : rest) = (40, rest)
oneStep ('X' : rest) = (10, rest)
oneStep ('I' : 'X' : rest) = (9, rest)
oneStep ('V' : rest) = (5, rest)
oneStep ('I' : 'V' : rest) = (4, rest)
oneStep ('I' : rest) = (1, rest)
oneStep (_: rest) = (0,rest)
 -- Non-exhaustive patterns

testCases = ["MCMXC", "MMVIII", "MDCLXVI"]
test = zip testCases (map toArabic testCases)




