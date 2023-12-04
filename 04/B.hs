import qualified Data.Text as T
import Prelude as P
import System.IO
import Data.Set as Set

strSplit :: [Char] -> [Char] -> [[Char]]
strSplit separator string = P.map T.unpack (T.splitOn (T.pack separator) (T.pack string))

digits = ['0'..'9']

parseSet string = fromList [read [d|d<-num, d `P.elem` digits] :: Int|num<-(strSplit " " string), length num > 0] :: Set Int

pointsFromMatches n = n

parseLine line = 
  let relevant = P.head(P.drop 1 (strSplit ": " line))
  in 
  let winningNumbers = P.head (strSplit " | " relevant) 
  in
  let drawnNumbers = P.head (P.drop 1 (strSplit " | " relevant))
  in
  pointsFromMatches (length (intersection (parseSet winningNumbers) (parseSet drawnNumbers)))

-- process :: (Int, [Int], [String])
process (score, open, lines) = if sum open == 0 then (score, open, lines) else
  let current = sum [1 | a<-open, a == 0]
  in
  let n = parseLine (lines !! (current))
  in
  let ncurrent = open !! current
  in
  let newOpen = [if i <= current then 0 else 
				 if i - current <= n then (open !! i) + ncurrent else open !! i
				 | i <- [0..(length open - 1)]] :: [Int]
  in
  let newScore = score + (n) * ncurrent
  in
  process (newScore::Int, newOpen, lines)

parseFile filename = do
  contents <- readFile filename
  return ( 
    let (score, _, _) = (process (0, P.take (length (lines contents)) (repeat 1), (lines contents)))
    in score + length (lines contents))
