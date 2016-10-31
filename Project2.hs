listA = [3, 5 ..]
listB = [x | x <- [3, 5 ..]]
listC = [x | x <- [3, 5 .. 5999]]
listD = [x | x <- [3, 5 .. 6000]]
listE = [x | x <- [3, 5 ..], (x < 6000)]
listF = [x | x <- takeWhile (< 6000) [3, 5 ..] ]


oddsFrom3 :: [Int]
oddsFrom3 = [3, 5 .. ]

primeDivisors :: Int -> [Int]
primeDivisors n = [d | d <- takeWhile ((<= n) . (^2)) primes, n `mod` d == 0]

primes ::  [Int]
primes = 2 : [p | p <- oddsFrom3, null (primeDivisors p)]

isPrime :: Int -> Bool
isPrime g = g == head (dropWhile (< g) primes)

--g= p + 2 * k^2
goldbachPairs :: Int -> [(Int, Int)]
goldbachPairs g = [(p, k) | k <- takeWhile ((< g) . (*2) . (^2)) [1 .. ], 
                            p <- [g - 2 * k * k], isPrime p] 
-- p=g-2*k^2

goldbachFails :: [Int]
goldbachFails = [g | g <- takeWhile (< 6000) oddsFrom3, not (isPrime g), 
                     null (goldbachPairs g) ]

iSqrt :: (Integral a, Integral b) => a -> b
iSqrt n = floor (sqrt (fromIntegral n))

isSquare :: Int -> Bool
isSquare n = (iSqrt n) ^ 2 == n 

goldbachPairs' :: Int -> [(Int, Int)]
goldbachPairs' g = [(p, iSqrt kSqr) | p <- takeWhile (< g) (tail primes),  kSqr <- [(g - p) `div` 2], isSquare kSqr]

--k^2= (g-p)/2

goldbachFails' :: [Int]
goldbachFails' = [g | g <- takeWhile (< 6000) oddsFrom3, not (isPrime g), 
                      null (goldbachPairs' g) ]

goldbachDiffs :: [Int]
goldbachDiffs = [g | g <- takeWhile (< 6000) oddsFrom3, not (isPrime g),  goldbachPairs g /= reverse (goldbachPairs' g)]
