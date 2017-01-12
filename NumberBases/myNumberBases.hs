module MyNumberBases where

{-

This project requires that some functions produce Integers. But 
some values are Int. Recall that Haskell is very picky about 
numeric types. For example the following produces an error 
because (+) expects its types to be the same.

> (1::Int) + (1::Integer)

<interactive>:418:13: error:
    • Couldn't match expected type ‘Int’ with actual type ‘Integer’
    • In the second argument of ‘(+)’, namely ‘(1 :: Integer)’
      In the expression: (1 :: Int) + (1 :: Integer)
      In an equation for ‘it’: it = (1 :: Int) + (1 :: Integer)

The following are both acceptable.

> (1::Int) + (1::Int) ==> 2
> (1::Integer) + (1::Integer) ==> 2

To perform an operation on an Int and an Integer, the
easiest solution is to wrap one of them with fromIntegral.
That allows Haskell to convert it to the required type.

> fromIntegral (1::Int) + (1::Integer) ==> 2
> (1::Int) + fromIntegral (1::Integer) ==> 2
The resulting values are of types Integer and Int respectively.

-}

import Data.List

-- N_ary, for n-ary, meaning to the base n, manipulates numbers
-- to various bases. The fields for N_ary objects are 
--   o  the string to the given base
--   o  the base as an Int
-- For example: the value 15 represented in base 16 is "F"
-- It is represented as a N_ary:     N_ary "F" 16.
-- To base 10 it is represented as:  N_ary "15" 10.
-- To base 2 it is represented as:   N_ary "1111" 2.
data N_ary = N_ary String Int 

-- The String representation of  N_ary <string> <base>
-- is:  <String>~~<base>. 
--   > N_ary "F" 16 ==> "F"~~16.
instance Show N_ary where
  <Your job>

-- The (~>) operator converts something into an N_ary object
-- with a specified base. 
infixl 5 ~> 

-- A type  a  belongs to the  ToBase  typeclass if it can be converted
-- to an N_ary object with a specified base. 
class ToBase a where
  (~>) :: a -> Int -> N_ary


-- Once we declare Integer a member of the ToBase type class 
-- any positive Integer can be converted into an N_ary object.
-- In the following, the first argument is always the value 10. 
-- Here is 10 in various bases.
--   > 10 ~> 16 ==> "A"~~16
-- The result of  10 ~> 16  is   N_ary "A" 16. 
-- But that is printed as   "A"~~16.
--   > 10 ~> 2  ==> "1010"~~2
--   > 10 ~> 5  ==> "20"~~5
--   > map (10 ~>) [1 .. 13]
--   ["1111111111"~~1,"1010"~~2,"101"~~3,"22"~~4,"20"~~5,"14"~~6,
--    "13"~~7,"12"~~8,"11"~~9,"10"~~10,"A"~~11,"A"~~12,"A"~~13]
-- Note that 10 to any base greater than 10 is "A".
--
-- Another example: 30 expressed in base 16 is "1E".
--   > 30 ~> 16 ==> "1E"~~16
-- Also:
--   > 35 ~> 36       ==> "Z"~~36   ('Z' is the largest digit.)
--   > 36 ~> 36       ==> "10"~~36
--   > 36 ~> 10       ==> "36"~~10
--
-- To implement this you must make Integer a member of the
-- ToBase class.
instance ToBase Integer where
-- There are three special cases.

-- Generate an error if the requested base is < 1 or > 36.
  _   ~> base 
    | base < 1 || base > 36 = error "Base must be in [1 .. 36]"

  -- A special case since no other leading zeros are generated. 
  -- Without this, for any n, would get: > 0 ~> n ==> ""~~n  
  0   ~> base = N_ary "0" base

  -- Another special case for base 1: > 5 ~> 1 ==> "11111"~~1
  int ~>    1 = N_ary (replicate (fromIntegral int) '1') 1

  -- The general case.
  <Your job>
  -- Hint: Use divMod and recursion.
 

-- We can make N_ary objects themselves a member of the ToBase class.
-- This makes it possible to convert an N_ary object to a different base.
--   > N_ary "FF" 16 ~> 10 ==> "255"~~10
--   > N_ary "F"  16 ~> 15 ==> "10"~~15
--   > N_ary "35" 10 ~> 36 ==> "Z"~~36
instance ToBase N_ary where
  <Your job>
  -- Hint: Use toInt to convert the N_ary object to an Integer.  
  -- Then use (~>) to go back to an N_ary object.


-- Let (~~) work as an operator for writing N_ary numbers. Then
-- output also works as input.
infix 9 ~~

-- For example, let  "F"~~16  mean  N_ary "F" 16.
-- When printed it will look like "F"~~16.
--   > "F"~~16 ==> "F"~~16
--
-- If the string uses digits greater than or equal to the base,
-- or lower case letters, convert the string to correct digits.
-- If you follow the Hint, these conversion happen automatically.
--   > "102"~~2 ==> "110"~~2
--   > "303"~~2 ==> "1111"~~2
--   > "f"~~16  ==> "F"~~16
-- This also lets you convert to another base on the fly.
--   > "Z"~~36 ~> 35 ==> "10"~~35
-- No parentheses are necessare because (~~) has higher
-- precedence than (~>)
(~~) :: String -> Int -> N_ary
<Your job>
-- Hint: Use the fact that N_ary belongs to the ToBase class. 
-- Construct an N_ary object according to the input.
-- Then use (~>) to create a validated version.


-- The digits. Treat upper and lower case as equivalent.
-- Use upper case when generating strings.
--   > "f"~~16 ==> "F"~~16
digits :: [Char]
digits = ['0' .. '9'] ++ ['A' .. 'Z'] ++ ['a' .. 'z']


-- Convert a Char (a "digit") to an Int
--   > digitToInt 'A' ==> 10
--   > digitToInt 'Z' ==> 35
--   > digitToInt 'a' ==> 10
--   > digitToInt '5' ==> 5
digitToInt :: Char -> Integer 
<Your job>
-- Hint: Use the function elemIndex.


-- An operator for adding two N_ary numbers
infixl 6 ~+ 

-- Add two N_ary numbers to get a third N_ary number.
-- Express the result to the smaller base.
--   > "E"~~16 ~+ "1"~~10 ==> "15"~~10
(~+) :: N_ary -> N_ary -> N_ary
<Your job>
-- Hint: First convert both N_ary objects to Integer.


-- Convert an N_ary object to an Integer. Can be used to 
-- export an N_ary number for use in a normal calculation.
--   > 3 + toInt ("101"~~2) ==> 8
-- Also used in:   instance ToBase N_ary
-- Can't use the name "toInteger" since it's taken.
toInt :: N_ary -> Integer
<Your job>
-- Hint: Use foldr.


{-
More examples

> "101"~~2 ~+ "1111"~~2
"10100"~~2

> "101"~~2 ~+ "1111"~~2 ~> 10
"20"~~10

> "E"~~16 ~+ "1"~~10
"15"~~10

> "E"~~16 ~+ "1"~~10 ~> 16
"F"~~16

> toInt ("D"~~16 ~+ "10"~~2)
15

> "D"~~16 ~+ "10"~~2 ~> 10
"15"~~10
No parentheses needed since (~+) has higher precedence than (~>). 

> "D"~~16 ~+ "10"~~2 ~> 16
"F"~~16

> "D"~~16 ~+ "10"~~2
"1111"~~2

> (10 ~> 1) ~+ "5"~~6 ~> 10 
"15"~~10

-}


