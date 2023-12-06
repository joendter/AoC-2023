import qualified Data.Text as T
import Prelude as P
import System.IO
import Data.List


strSplit :: [Char] -> [Char] -> [[Char]]
strSplit separator string = P.map T.unpack (T.splitOn (T.pack separator) (T.pack string))

digits = ['0'..'9']

distances time = [t*(time-t)| t<-[1..time]]

compa [time,distance] = sum [1| t<- (distances time), t > distance]

readInts string = [read w ::Integer| w<-(strSplit " " string), length w > 0]


parseFile filename = do
  contents <- readFile filename
  return (
	let processed = [c| c<-contents, c `elem` digits ||  c == '\n']
	in 
	let timesRaw:distancesRaw:_ = strSplit "\n" processed
	in
	let (times, distances) = (readInts timesRaw, readInts distancesRaw)
	in
	foldl (*) 1 (map compa (transpose [times,distances])

	))
