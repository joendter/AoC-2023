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


move :: String -> [Char] -> [String] -> Int -> Int
move currentPosition (currentMove:moves) lines steps = let parsefun x = foldl foldingHelper x [parseLine line| line<-lines]
  in
  let nextPosition = fst (parsefun (currentPosition, currentMove)) 
  in
  if nextPosition == "ZZZ" then steps + 1 else
	move nextPosition (moves ++ [currentMove]) lines (steps + 1)

parseFile contents= 
  (let moves:_:lines = strSplit "\n" contents
	in  move "AAA" moves [l|l<-lines, length l > 0] 0)

main :: IO()
main =  do
  contents <- readFile "input"
  putStrLn (show (parseFile contents))
