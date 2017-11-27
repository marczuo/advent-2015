module Advent2015.Days.Day20 where

divisors :: Int -> [Int]
divisors n = let halfDivs = filter ((==0) . (rem n)) [1 .. intSqrt n]
                 intSqrt = floor . sqrt . fromIntegral in
                 halfDivs ++ (map (div n) $ reverse halfDivs)

sigmaUpperBound :: Double -> Int
sigmaUpperBound n = let eulerMascheroni = 0.5772156649015328606065121
                        expGamma = exp eulerMascheroni in
                        floor $ expGamma * n * log (log n)

-- Note: This assumes Riemann's hypothesis is true!
minimumSigma :: Int -> Int
minimumSigma s = let generalSearch = filter ((>s) . sum . divisors) $ filter ((>s) . sigmaUpperBound . fromIntegral) [nL+1..]
                     partialSearch = filter ((>s) . sum . divisors) [1..nL]
                     nL = 5040 in
                     if null partialSearch then head generalSearch else head partialSearch

-- part2
calculateGifts :: Int -> Int
calculateGifts n = sum $ map (\e -> if div n e > 50 then 0 else e*11) $ divisors n

minimumHouse :: Int -> Int
minimumHouse s = head $ filter ((>s) . calculateGifts) [1..]

part1, part2 :: String -> Int
part1 = minimumSigma . (`div` 10) . read
part2 = minimumHouse . read
