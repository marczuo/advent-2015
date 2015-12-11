import Local.IO.AdventOfCode

main :: IO ()
main = adventIO today parseContent part1 part2 where
    today = "1" 
    step '(' = succ; step ')' = pred 
    parseContent = head . lines
    part1 = foldl (flip step) 0
    part2 = length . takeWhile (>=0) . scanl (flip step) 0
