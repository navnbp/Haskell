ints2 = [3,5..6000]

smallDivs n = [d | d <- takeWhile (<= n `div` 2) ints2, n `mod` d == 0]

primes = [p | p <- ints2, smallDivs p == []] 

isASquare sq = truncate ( sqrt(x)) * truncate ( sqrt(x)) == sq where x = fromIntegral sq

isPrime prm =  prm `elem` primes

valid g = [p | p <- primes,  isASquare (div (g - p)  2)] == []

solution = [z | z <- ints2, not (isPrime z), valid z]

--nonPrime = [y | y <- ints2, not (isPrime y)]

--solution = [z | z <- nonPrime, valid z]

--oddNonPrime g = [y | y <- ( takeWhile (<= g ) primes )  ,isASquare (( g - y) `div` 2)

--solution = [x | x <- ints2, isPrime x = False , odd x = True , oddNonPrime x == []]

-- getPrimes x = takeWhile(<= x) primes

--oddPrime = [|  <= takeWhile(<6000), odd == True]

-- oddPrime ::   Bool

-- oddPrime  op = [g | g <- [1..6000] , g 'elem' p True]  
