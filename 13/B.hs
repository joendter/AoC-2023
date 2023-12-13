import qualified Data.Text as T
import Prelude as P
import System.IO
import qualified Data.List
import qualified Data.Set as Set

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

variants image = [
  [[ if xx == x && yy == y then 
  if (image !! yy) !! xx == '#' then '.' else '#'
  else (image !! yy) !! xx
  |xx<- [0..(length (head image) -1)] ] |yy<- [0..(length image-1)]]
  |y<-[0..(length image-1)], x <- [0..(length (head image) -1)] ]

getMirrorLines :: [[Char]] -> Set.Set (Int, Char)
getMirrorLines image = Set.fromList(
  [(y, 'y') | y<-[1..(length image -1)], isMirrorAtRow y image]  ++
  [(x, 'x') | x<-[1..(length (head image) - 1)], isMirrorAtCol x image])

scoreImage :: [[Char]] -> Int
scoreImage image = let baseLines = getMirrorLines image
  in
  let founds = (foldl Set.union Set.empty [ getMirrorLines variant | variant<-(variants image)]) `Set.difference` baseLines
  in
  sum [if typeo == 'y' then 100*x else x | (x,typeo) <- Set.toList founds ]



parseFile filename = do
  contents <- readFile filename
  return (
	let imgs = [[l|l<-(strSplit "\n" img), length l > 0] |img<-(strSplit "\n\n" contents), length img > 0]
	in
	sum [scoreImage img| img<-imgs]	
	  )
