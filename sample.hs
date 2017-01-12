import Data.List.Split

data Player = X | O deriving (Eq, Show)

-- ?? What are some example Outcome objects?
data Outcome = Winner Player | Draw 
instance Show Outcome where
  show (Winner p) = show p ++ " wins."
  show Draw       = "Drawn game."

data Mark = Mark Player | Empty deriving Eq
instance Show Mark where
  show (Mark p) = show p
  show Empty    = " "

 
-- solveRPN =foldl (\ x y => ) map (splitOn " ") . (splitOn ",") -- 


solve = splitOn "," . filter (/= ' ') 


-- f=foldl (\a b -> (a++",")) [] 

 -- splitOn "," 








--  x= map nextStep "fasdsd            dfasg"
-- nextStep :: Char -> Char
-- nextStep c
--  | c == ' ' = ';'
--  | c == ',' = ';'
--  | otherwise = c


-- replace c
--  | c == ' ' = ';'
--  | c == ',' = ';'
--  | otherwise = c

