module TicTacToe (Game, next, newGame, play, start, startAt) where

-- Based on a program by Peter Drake. 

import Data.List
import Data.Maybe
import Data.Ord

-- Constants and Types
e = ' ' -- The empty cell 
o = 'O'
x = 'X'

-- Use 1-based indexing throughout except when dealing with actual lists.
rowColRange = [1 .. 3]

-- A Row is three Chars.
type Row   = [Char] --  [Char, Char, Char]
-- The Board is three Rows.
type Board = [Row]  -- [[Char, Char, Char], 
                    --  [Char, Char, Char], 
                    --  [Char, Char, Char]]

-- The primary data structure. Keeps track of the game state.
-- The minimaxValue is the value of the game computed by minimax.
data Game = Game { board        :: Board, 
                   --  1 means X can force a win.
                   --  0 means each side can force a draw.
                   -- -1 means O can force a win.
                   minimaxValue :: Int,  
                   errMsgs      :: [String], 
                   maxDepth     :: Int       } deriving (Eq)

-- The initial Game state.
newGame :: Game
newGame = Game { board        = replicate 3 [e, e, e], 
                 -- We know the initial minimaxValue is 0. 
                 -- Would take too long to compute it.
                 minimaxValue = 0, 
                 errMsgs      = [], 
                 maxDepth     = 9                      } 

-- Define the show function (like Java's toString() function) for a Game.
instance Show Game where
  -- First generate a list of Strings, one for each line of output. 
  -- Then add "\n     " between Strings and at the start and end.
  -- When concatenated this will both indent and add "\n" to each line. 
  -- Finally concat the Strings into a single String.
  show = concat . surround "\n     " . makeOutputStrings  

    where 
    -- Add a separator between elements and at the start and end of a list.
    -- In this program, the list and separator are Strings, but that's not
    -- required by surround.
    surround :: a -> [a] -> [a]
    surround = \separator ->
                 foldr (\element acc -> separator : element : acc) [separator]

    -- Construct the output as a List of Strings by combining the
    -- error messages, the board, and the game status String.
    makeOutputStrings :: Game -> [String]
    makeOutputStrings = \game ->  errMsgs game 
                                  ++ makeBoardStrings (board game)
                                  ++ [gameStatus game]
      where
      -- Return a String indicating the status of the Game.
      gameStatus :: Game -> String                    
      gameStatus = 
        \game ->
          case gameOver game of
          -- gameOver game will return Just <something> if the game is over.                
          Just '-' -> "Tie game."
          Just p   -> p : " wins."  -- Pattern matches p.
          -- The game isn't over.
          Nothing  -> (whoseMove game) : " to play. "
                      ++ case minimaxValue game of
                         1    -> "X can force a win."
                         (-1) -> "O can force a win."
                         0    -> "Both sides can force a draw."

      -- Format the board for printing, but keep as a list of Strings.
      makeBoardStrings :: Board -> [String] 
      makeBoardStrings = surround "+---+---+---+" . map makeRowString 
        where
        -- Convert a row (a list of 3 Chars) into a String with dividers.
        -- E.g., ['X', ' ', 'O'] => "| X |   | O | "
        makeRowString :: Row -> String
        makeRowString = concat . surround "| " . map (:" ") 

-- End Constants and Types

cellIsEmpty :: Int -> Int -> Game -> Bool
cellIsEmpty = \r c game -> board game !! (r-1) !! (c-1) == e
                                    
-- Count the number of p's in the board. (p is for player.)
count :: Char -> Game -> Int
count = \p game -> length [c | row <- board game, c <- row, c == p]

-- Find the minimaxValue and maxDepth of game using minimax.
-- maxDepth is the longest possible game with this outcome.
evaluate :: Game -> Game
evaluate = 
  \game -> 
    -- If the game is over, set the minimaxValue based on the outcome.
    case gameOver game of
    Just 'X' -> game {minimaxValue = 1,    maxDepth = 0}
    Just '-' -> game {minimaxValue = 0,    maxDepth = 0}
    Just 'O' -> game {minimaxValue = (-1), maxDepth = 0}
    -- The game is not over. Call minimax to explore the subtree.
    Nothing  -> let minimaxGame = minimax game                             
                in game {minimaxValue = minimaxValue minimaxGame, 
                         maxDepth     = 1 + maxDepth minimaxGame}

-- Return Maybe Char based on the current game state. 
--   Just 'X': x has 3 in a row
--   Just 'O': o has 3 in a row
--   Just '-': the board is full but no 3-in-a-row
--   Nothing:  none of the above
gameOver :: Game -> Maybe Char
gameOver = 
  \game ->
    case findWinner (board game) of
    Just p  -> Just p  -- p is a player
    Nothing -> if boardIsFull game then Just '-' else Nothing
               where boardIsFull = (0 ==) . count e
      
  -- Can we find 3 identical player marks in a row?
    where 
    findWinner :: Board -> Maybe Char     
    findWinner =
      \board ->
          -- searchSpace is all the possible 3-in-a-row combinations.
        let searchSpace :: [Row] -- really [ [Char, Char, Char], ... ]
            searchSpace = diag board : revDiag board : board ++ myTranspose board
              where 
              diag :: [[a]] -> [a] -- The major diagonal
              diag xss = [xss !! i !! i | i <- [0 .. (length xss - 1)]]

              -- transpose is a library function. It may be defined as follows.
              myTranspose :: [[a]] -> [[a]]
              myTranspose xss = [map (!!i) xss | i <- [0 .. (length xss - 1)]]

              revDiag :: [[a]] -> [a] -- The minor diagonal
              revDiag = diag . map reverse

         in safeHead [x | [x,y,z] <- searchSpace, x == y && y == z && z /= e]

         where 
         safeHead :: [a] -> Maybe a
         safeHead []    = Nothing
         safeHead (x:_) = Just x
         
-- Find and return the best successor of game for the current player. 
-- Best means highest/lowest minimaxValue for 'X'/'O' and most remaining
-- moves among those with the same minimaxValue. 
-- Note: to compare games we compare (minimaxValue, maxDepth) pairs.
minimax :: Game -> Game
minimax game =
  minOrMaxOn (\g -> (minimaxValue g, maxDepth g * selectByPlayer 1 (-1)))
             (map evaluate $ successors game)
  where 
  -- Select 1st or 2nd arg depending on whose move it is.
  selectByPlayer :: a -> a -> a
  selectByPlayer = 
    \onXMove onOMove ->          
      case whoseMove game of 'X' -> onXMove -- Why can't we use x and o
                             'O' -> onOMove -- in these case branches?
  minOrMaxOn f = selectByPlayer (maximumBy (comparing f))
                                (minimumBy (comparing f)) 

  -- Return a list of successor Games with all possible next moves. 
  -- (Values are not computed.)
  successors :: Game -> [Game]
  successors = \game -> [placeMark r c game | r <- rowColRange, 
                                              c <- rowColRange, 
                                              cellIsEmpty r c game]

-- Make the best move for the current player from the given game position.
next :: Game -> Game
-- If the game is starting or over, start a new game
next = \game -> if count e game == 9 || isJust (gameOver game) 
                then start
                else minimax game

-- Place the mark of the current player at position r, c. No evaluation.
placeMark :: Int -> Int -> Game -> Game
placeMark =
  \r c game -> -- Replace an existing row with a new row.
    newGame {board = replace (r-1) -- The next line is the new row r 
                     (replace (c-1) (whoseMove game) (board game !! (r-1)))
                     (board game)}
    where 
    -- Replace the element at position i in items with item      
    replace :: Int -> a -> [a] -> [a]
    replace = \i item items -> let (xs, _:ys) = splitAt i items 
                               in xs ++ item:ys

-- Play at position r, c and evaluate the result. 
play :: Int -> Int -> Game -> Game
play r c game
  | -- If (gameOver game) returns Just <something> the game is over. 
    -- Start a new game at the requested position.
    isJust (gameOver game)     = startAt r c 
  | not (elem r rowColRange)   = game {errMsgs = ["Row out of range."]}
  | not (elem c rowColRange)   = game {errMsgs = ["Column out of range."]}
  | not $ cellIsEmpty r c game = game {errMsgs = ["That square is taken."]}  
  | otherwise                  = evaluate $ placeMark r c game

-- Start the game. X always starts in the upper left corner.
start :: Game
start = startAt 1 1 

-- Start the game at r c. 
startAt :: Int -> Int -> Game
startAt = \r c -> play r c newGame

whoseMove :: Game -> Char
whoseMove = \game -> if count x game <= count o game then x else o
