import qualified Data.Text as T
import Prelude as P
import System.IO
import qualified Data.List

strSplit :: [Char] -> [Char] -> [[Char]]
strSplit separator string = P.map T.unpack (T.splitOn (T.pack separator) (T.pack string))

digits = ['0'..'9']
letters = ['A'..'Z']

parseLine :: String -> ((String, Char) -> (String, Char))
parseLine line = let [origin, left, right] = [w|w<-(strSplit " " [c|c<-line, c `elem` letters || c == ' ' ]), length w > 0]
  in
  let fun (x, lrp) = let processed = lrp == 'P'
	in if x == origin && not processed then if lrp == 'L' then (left, 'P') else (right, 'P') else (x, lrp)
  in fun

foldingHelper :: (String, Char) -> ((String, Char) -> (String, Char)) -> (String, Char)
foldingHelper x f = f x

parsefunFactory :: [String] -> ((String, Char) -> (String, Char))
parsefunFactory lines = let parsefun x = foldl foldingHelper x [parseLine line | line<-lines] 
  in parsefun

move :: String -> [Char] -> ((String, Char) -> (String, Char)) -> Int -> Int
move currentPosition (currentMove:moves) parsefun steps = let nextPosition = fst (parsefun (currentPosition, currentMove)) 
  in
  if nextPosition == "ZZZ" then steps + 1 else
	move nextPosition (moves ++ [currentMove]) parsefun (steps + 1)

parseFile :: String -> Int
parseFile contents= do
  return (let (moves:_:lines) = strSplit "\n" contents
	in (move "AAA" moves (parsefunFactory [l|l<-lines, length l > 0])  (0::Int)))

main :: IO()
main = do
  contents <- readFile "input" 
  print (parseFile contents)
