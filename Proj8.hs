import Data.List    
import Data.List.Split

solveRPN :: String -> Float  
solveRPN = head . foldl foldingFunction [] . splitByDelimeter --splitOn "," . filter (/= ' ')   
    where   foldingFunction (x:y:ys) "*" = (y * x):ys  
            foldingFunction (x:y:ys) "+" = (y + x):ys  
            foldingFunction (x:y:ys) "-" = (y - x):ys  
            foldingFunction (x:y:ys) "/" = (y / x):ys  
            foldingFunction (x:y:ys) "^" = (y ** x):ys  
            foldingFunction (x:xs) "ln" = log x:xs  
            foldingFunction xs "sum" = [sum xs]  
            foldingFunction xs numberString = read numberString:xs 


splitByDelimeter :: String -> [String]
splitByDelimeter  = filter (/="") . splitOn ";" . map change   
   where 
     change c
      | c == ' ' = ';'
      | c == ',' = ';'
      | otherwise = c




{-
10 3 +

 words 


"10 3, + 4, -"

Test with:
 solveRPN "10;;4;;;;;3;;;+, 2,*,-"
"10","4","","",
 "10,4,3,+,2,*,-"

 ["10","4","3","+","2","*","-"]

splitOn "," . filter (/= ' ') 
-}