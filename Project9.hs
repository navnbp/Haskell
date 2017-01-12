import Data.Function (on)
import Data.List (intersperse, sort)

-- The A side, the B side, or a Crossover.
data Label = A | B | C deriving (Eq, Show)

-- A Step is a transition from one intersection to another.
-- It is on the A or B side, or it's a crossover. In all
-- cases it has an associated distance.
data Step = Step Label Int

instance Show Step where
  show (Step lbl dist) = show lbl ++ "/" ++ show dist

--   A Path is a sequence of Steps.
data Path = Path [Step]

-- The distance for a Path is the sum of the Step distances.
dist :: Path -> Int
dist (Path steps) = sum $ map (\(Step _ d) -> d) steps 
-- The following is also correct.
-- dist (Path steps) = foldl (\sum (Step _ d) -> sum+d) 0 steps

-- This is how foldl works.
myFoldl :: (a->b->a)->a->[b]->a
myFoldl _ acc [] = acc
myFoldl f acc (x:xs) = myFoldl f (f acc x) xs

-- measureOfMerit is a metric for how good a path is. The 
-- shorter the distance the better. If two Paths have the 
-- same distance, fewer steps is better.
measureOfMerit :: Path -> (Int, Int)
measureOfMerit p@(Path steps) = (dist p, length steps) 

-- Two Paths are equal if their measures of merit are equal.
instance Eq Path where
  (==) = (==) `on` measureOfMerit
 
-- Compare paths by comparing their measures of merit.
instance Ord Path where
  compare = compare `myOn` measureOfMerit 

-- This is how `on` works.
myOn :: (b -> b -> c) -> (a -> b) -> a -> a -> c
f1 `myOn` f2 = \x y -> f1 (f2 x) (f2 y)  


-- Notice that in the start and end functions below there 
-- is no use of the functions head, tail, last, or init. 
-- Also, there are no let or where statements. It's all 
-- done with pattern matching and case statements. The
-- only function call is reverse.

-- The start of a path is the label of the first step.
start :: Path -> Label
start (Path (Step label _ : _)) = label

-- If the label of the last step is A or B, that's the 
-- value for end. If the last step is C, the path ends 
-- at the opposite of the previous step.
end :: Path -> Label
end (Path steps) = 
  case reverse steps of
  Step C _ : rest     -> case rest of Step A _ : _ -> B 
                                      Step B _ : _ -> A 
  Step lastLbl _ : _  -> lastLbl
  
instance Show Path where
  show p@(Path steps) = 
    "\n" ++ show (start p) ++ "->" ++ show (end p)
    ++ ". Dist: " ++ show (dist p) ++ ". "
    ++ (concat . intersperse ", " . map show) steps

-- Can two paths be joined, i.e., does the first one end
-- where the second one starts?
(>?<) :: Path -> Path -> Bool
p1 >?< p2 = end p1 == start p2

-- Extend one path by another by appending their Steps.
-- Should not be used unless the Paths can be joined.
(+>) :: Path -> Path -> Path
-- A guard may be used to check the >?< constraint.
p1@(Path steps1) +> p2@(Path steps2) 
  | p1 >?< p2 = Path (steps1 ++ steps2)
  | otherwise = error ("Attempt to join " ++ show (end p1)
                       ++ " to " ++ show (start p2) ++ " in "
                       ++ show p1 ++ " +> " ++ show p2 ++ "\n")


-- A QuadPaths object always has exactly 4 paths connecting from
-- one cross link to another. It has the four optimal paths for: 
-- A-to-A, A-to-B, B-to-A, B-to-B.
data QuadPaths = QuadPaths { paths :: [Path] }

-- Extend one QuadPaths object by another by joining the paths that
-- fit and selecting the best path for each start-end combination. 
-- E.g. A-to-B can be either A-to-A +> A-to-B or A-to-B +> B-to-B.
(++>) :: QuadPaths -> QuadPaths -> QuadPaths
QuadPaths paths1 ++> QuadPaths paths2 =
  QuadPaths (map bestWithStartEnd [(A, A), (A, B), (B, A), (B, B)])
  where bestWithStartEnd :: (Label, Label) -> Path
        bestWithStartEnd (s, e) =
          minimum [p1 +> p2 | p1 <- paths1, s == start p1,
                              p2 <- paths2, e == end p2, 
                              p1 >?< p2]

-- Input to the overall problem is given as a list of Ints. The 
-- list should be understood as sequences of three Steps.
-- Each 3-Step sequence is (a) the distance along the A Step,
-- (b) the distance along the B Step, and (c) the distance of the
-- crossover Step that connect the ends of the other two Steps.

-- The problem is to find all 4 shortest paths from Heathrow 
-- to London. When sorted the head is the minimum. 

-- The program first uses qpList to convert the initial data into 
-- a sequence of QuadPaths. Then it uses combinePaths to combine 
-- the list of QuadPaths into a single QuadPath. Then it sorts the 
-- paths in that QuadPath.
optimalPaths :: [Int] -> [Path]
--         [path] [path] QuadPaths     [QuadPaths]   [Int]
optimalPaths = sort . paths . combinePaths . qpList 
-- optimalPaths routeData = sort paths where 
--   QuadPaths paths = combinePaths $ qpList routeData
  where 
   
  -- combinePaths glues a list of QuadPaths objects together.

  -- Gluing two QuadPaths together produces a QuadPaths object 
  -- with four optimum paths (A-to-A, A-to-B, B-to-A, B-to-B)
  -- over the distance of the two objects together. (See ++>.)

  -- Any of the following versions of combinePaths works.

  -- combinePaths :: [QuadPaths] -> QuadPaths
  -- combinePaths = foldl1 (++>) 
  
  -- combinePaths :: [QuadPaths] -> QuadPaths
  -- combinePaths = foldr1 (++>)

  -- The following version is longer, but it lends itself to
  -- parallel processing. It also illustrates that the sequence
  -- in which the QuadPaths objects are glued together doesn't
  -- matter.
  --
  -- Split the QuadPaths list into two lists; combine the
  -- QuadPaths in each list; combine the two results.
  combinePaths :: [QuadPaths] -> QuadPaths
  combinePaths qps
    | length qps <= 3 = foldl1 (++>) qps
    | otherwise       = combinePaths leftQPs ++> combinePaths rightQPs
        where (leftQPs, rightQPs) = splitAt (length qps `div` 2) qps

  -- Read the inputs 3 at a time and make a QuadPaths of each group.
  -- Each QuadPaths object is the 4 paths for a segment.
  qpList :: [Int] -> [QuadPaths]
  qpList (aDist : bDist : cDist : rest) = 
    QuadPaths [ Path [Step A aDist],               -- A to A
                Path [Step A aDist, Step C cDist], -- A to B
                Path [Step B bDist, Step C cDist], -- B to A
                Path [Step B bDist]                -- B to B
              ] : qpList rest
  qpList [] = []
  -- What to do if there are not enough input elements.
  -- These are arbitrary decisions.
  qpList [aDist, bDist] = qpList [aDist, bDist, 0]
  qpList [xDist]        = qpList [xDist, xDist, 0]

{- 
   To test using the example in the book.
   > optimalPaths [50, 10, 30, 5, 90, 20, 40, 2, 25, 10, 8] 
   [
   B->B. Dist: 75. B/10, C/30, A/5, C/20, B/2, B/8,
   B->A. Dist: 75. B/10, C/30, A/5, C/20, B/2, B/8, C/0,
   A->B. Dist: 85. A/50, A/5, C/20, B/2, B/8,
   A->A. Dist: 85. A/50, A/5, C/20, B/2, B/8, C/0]


   Why does changing the second crossover from 15 to 14 in
   the following examples cause the shortest path to take 
   the first crossover?

   > optimalPaths [10, 50, 30, 50, 5, 15, 2, 40, 0, 10, 5]  
   [
   A->B. Dist: 67. A/10, A/50, A/2, C/0, B/5,
   A->A. Dist: 67. A/10, A/50, A/2, C/0, B/5, C/0,
   B->B. Dist: 77. B/50, B/5, C/15, A/2, C/0, B/5,
   B->A. Dist: 77. B/50, B/5, C/15, A/2, C/0, B/5, C/0]

   > optimalPaths [10, 50, 30, 50, 5, 14, 2, 40, 0, 10, 5]  
   [
   A->B. Dist: 66. A/10, C/30, B/5, C/14, A/2, C/0, B/5,
   A->A. Dist: 66. A/10, C/30, B/5, C/14, A/2, C/0, B/5, C/0,
   B->B. Dist: 76. B/50, B/5, C/14, A/2, C/0, B/5,
   B->A. Dist: 76. B/50, B/5, C/14, A/2, C/0, B/5, C/0]

-}
