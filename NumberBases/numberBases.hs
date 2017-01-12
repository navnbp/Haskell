module NumberBases where


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
Note that parentheses aren't needed since (~+)
has higher precedence than (~>). So (~+) goes first.
If the requested base is > 36, issue an error message.

> "D"~~16 ~+ "10"~~2 ~> 16
"F"~~16

> "D"~~16 ~+ "10"~~2
"1111"~~2

> (10 ~> 1) ~+ "5"~~6 ~> 10 
"15"~~10

-}


