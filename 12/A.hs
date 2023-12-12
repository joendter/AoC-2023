import qualified Data.Text as T
import Prelude as P
import System.IO
import qualified Data.List

strSplit :: [Char] -> [Char] -> [[Char]]
strSplit separator string = P.map T.unpack (T.splitOn (T.pack separator) (T.pack string))

digits = ['0'..'9']

interleave :: [String] -> [String] -> String
interleave xs ys = 
  let zipped = zip (init xs) ys
  in
  concat [ a++b |(a,b)<-zipped ] ++ last xs


isValid string =
  let dat:sequencesString:_ = strSplit " " string
  in
  let sequences = [read num ::Int|num <-(strSplit "," sequencesString)]
  in sequences == fst (seqs dat)

seqs :: [Char] -> ([Int], Bool)
seqs string = if length string == 0 then ([],False) else
  let c = last string
  in
  let rest = init string
  in
  let (prev, continue) = seqs rest 
  in
  if (c == '.') then (prev, False) else
  if continue then (init prev ++ [last prev + 1], True) else (prev ++ [1], True)

variations :: [Char] -> [[Char]]
variations stri = 
  let split = strSplit "?" stri
  in 
  let len = length split - 1
  in
  [interleave split [if ((i `div` (2^j)) `mod` 2) == 0 then "." else "#" |j<-[0..len]] |i<-[0..(2^len - 1::Int) ]]

validVariations stri = 
  sum [1| variation <- variations stri, isValid variation]

main = do
  contents <- readFile "input"
  print (sum [validVariations line |line<-(lines contents), length line > 0])
