import qualified Data.Text as T
import Prelude as P
import System.IO
import qualified Data.List

strSplit :: [Char] -> [Char] -> [[Char]]
strSplit separator string = P.map T.unpack (T.splitOn (T.pack separator) (T.pack string))

digits = ['0'..'9']

isMirrorAtRow :: Int -> [[Char]] -> Bool
isMirrorAtRow row picture = let left = take row picture
  in
  let right = drop row picture
  in
  let width = min (length left) (length right)
  in
  take width (reverse left) == take width right

isMirrorAtCol :: Int -> [[Char]] -> Bool
isMirrorAtCol col picture = isMirrorAtRow col (Data.List.transpose picture)


scoreImage image = 100*(sum [i | i<-[1..(length image -1)], isMirrorAtRow i image]) + 1* (sum [i | i<-[1..(length (head image) - 1)], isMirrorAtCol i image])

parseFile filename = do
  contents <- readFile filename
  return (
	let imgs = [[l|l<-(strSplit "\n" img), length l > 0] |img<-(strSplit "\n\n" contents), length img > 0]
	in
	sum [scoreImage image | image<-imgs]	
	  )
