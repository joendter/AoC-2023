import qualified Data.Text as T
import Prelude as P
import System.IO
import qualified Data.List

strSplit :: [Char] -> [Char] -> [[Char]]
strSplit separator string = P.map T.unpack (T.splitOn (T.pack separator) (T.pack string))

digits = ['0'..'9']

finished :: [Int] -> Bool
finished values = minimum values == maximum values && head values == 0

diffs :: [Int] -> [Int]
diffs values = [ (values!!i) - (values!!(i-1))| i <- [1..(length values -1)] ]

process :: [[Int]] -> [[Int]]
process prev = let current = last prev
  in
  if (finished current) then prev else
  let neoPrev = prev ++ [diffs current]
  in
  process neoPrev

predict prev = sum [last i | i<-prev]

main = do
  contents <- readFile "input"
  print (sum [ predict (process ([[read n ::Int |n<-(strSplit " " line) ]]))| line<-(strSplit "\n" contents), length line > 0 ])
