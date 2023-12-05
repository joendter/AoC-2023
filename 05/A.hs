import qualified Data.Text as T
import Prelude as P
import System.IO

strSplit :: [Char] -> [Char] -> [[Char]]
strSplit separator string = P.map T.unpack (T.splitOn (T.pack separator) (T.pack string))

digits = ['0'..'9']

parseLine line = 
  let [destination, origin, range] = [read z ::Integer|z <-(strSplit " " line)]
  in
  let mapping (x, processed) = if processed then (x, processed) else if (origin <= x && x < origin + range) then (x - origin + destination, True) else (x, False)
  in mapping

foldingHelper n fun = (fun n)

parseChunk lines =
  let mapping x = fst (foldl foldingHelper (x, False) [parseLine line | line<-lines])
  in mapping

parseChunks chunks =
  let mapping x = foldl foldingHelper x [parseChunk chunk | chunk<-chunks]
  in mapping


parseFile filename = do
  contents <- readFile filename
  return ( 
	let _:seedsstr:chunksUnsplit = strSplit ":" [c|c<-contents, (c `elem` digits) || c == '\n' || c == ':' || c == ' ']
	in
	let seeds = [read s :: Integer| s<-(strSplit " " seedsstr), length s > 0]
	in
	let chunks = [[c| c<-strSplit "\n" chunk, c /= " ", length c > 0] | chunk <- chunksUnsplit]
	in map (parseChunks chunks) seeds )
