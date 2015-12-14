import System.IO

-- Note that there is no input file for day 10 because the input is fixed for everyone

-- We use this fact to our advantage, since the fixed input is element 87 Francium in
-- Conway's table of elements, and thus we don't need to first evolve the input string
-- into a Conway standard form

today = "10"

evolutionTable = [[],[1],[72,91,1,20,3],[2],[32,20,3],[4],[5],[6],[7],[8],[9],[10],[61,11],[12],[13],
        [67,14],[15],[16],[17],[18],[19], [67,91,1,20,27],[21],[22],[23],[24,14],[25],[26],[30,27],[28],
        [29],[63,20,89,1,20,30],[67,31],[32,11],[33],[34],[35],[36],[37],[38,92],[39,1,20,43],[68,40],
        [41],[42],[63,20,43],[67,44],[45],[46],[47],[48],[49],[61,50],[63,20,51],[67,52],[53],[54],[55],
        [56],[57,1,20,27],[58],[59],[60],[61,20,30],[62],[63,20,27],[67,64],[65],[66],[67,61],[68,20,27],
        [69],[70],[71],[72,91,1,20,74],[73],[32,20,74],[75],[76],[77],[78],[79],[80],[81],[61,82],[83],
        [84],[67,85],[86],[87],[88],[89],[90],[91]]

lengthTable = [0,2,32,27,42,34,28,24,18,14,12,9,10,10,7,12,10,6,4,4,2,16,14,8,5,12,8,5,8,6,3,17,23,26,20,
              16,14,10,7,7,23,28,20,15,21,24,18,12,10,8,5,7,13,18,14,8,6,5,10,8,6,3,6,7,11,16,12,7,9,14,10,
              6,5,32,27,42,34,28,24,18,14,12,9,10,10,7,12,10,6,4,4,2,1]

termwiseSum :: [[Integer]] -> [Integer]
termwiseSum [] = []; termwiseSum [x] = x
termwiseSum (x:xs) = zipWith (+) x (termwiseSum xs)

conway :: [[Integer]]
conway = []:[lengthTable !! seed : termwiseSum (map (conway !!) (evolutionTable !! seed)) | seed <- [1..92]]

main :: IO ()
main = putStr "day10p1: " >> print part1 >>
       putStr "day10p2: " >> print part2 where
    seed = 87
    part1 = conway !! seed !! 40
    part2 = conway !! seed !! 50
