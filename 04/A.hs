import qualified Data.Text as T
import Prelude as P
import System.IO
import Data.Set as Set

strSplit :: [Char] -> [Char] -> [[Char]]
strSplit separator string = P.map T.unpack (T.splitOn (T.pack separator) (T.pack string))

digits = ['0'..'9']

parseSet string = fromList [read [d|d<-num, d `P.elem` digits] :: Int|num<-(strSplit " " string), length num > 0] :: Set Int

pointsFromMatches n = if n == 0 then 0 else 2^(n-1)

parseLine line = 
  let relevant = P.head(P.drop 1 (strSplit ": " line))
  in 
  let winningNumbers = P.head (strSplit " | " relevant) 
  in
  let drawnNumbers = P.head (P.drop 1 (strSplit " | " relevant))
  in
  pointsFromMatches (length (intersection (parseSet winningNumbers) (parseSet drawnNumbers)))


parseFile filename = do
  contents <- readFile filename
  return (sum [parseLine line | line <- P.lines contents, length line > 0])
