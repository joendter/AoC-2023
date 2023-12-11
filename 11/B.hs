import qualified Data.Text as T
import Prelude as P
import System.IO
import qualified Data.List as L

strSplit :: [Char] -> [Char] -> [[Char]]
strSplit separator string = P.map T.unpack (T.splitOn (T.pack separator) (T.pack string))

digits = ['0'..'9']

galaxyExpansionHelper :: [String] -> String -> [String]
galaxyExpansionHelper prevGalaxies galaxy = if sum [1|i<-galaxy, i == '#'] > 0 then prevGalaxies ++ [galaxy] else prevGalaxies ++ [take (length galaxy) (repeat '+')]

processedGalaxies galaxies = let horizontal = foldl galaxyExpansionHelper [] galaxies
  in
  foldl galaxyExpansionHelper [] (L.transpose horizontal)

vecadd (xa,ya) (xb, yb) = (xa + xb, ya + yb)

vecidx arr (x,y) = (arr!!y)!!x


findGalaxies space = [(x,y)| y<- [0..(length space - 1)], x<- [0..(length (space!!0)-1)], vecidx space (x,y) == '#']

safeInit arr = if length arr == 0 then [] else init arr

safeTail arr = if length arr == 0 then [] else tail arr

adjsum :: [Char] -> Int
adjsum stri = sum [if (i == '+') then 10^6 else 1 |i<-stri]

distance (xaa,yaa) (xbb,ybb) space = let (xa, xb, ya, yb) = (min xaa xbb, max xaa xbb, min yaa ybb, max yaa ybb)
  in
  adjsum [vecidx space (xa,y) | y<-[ya..yb]] + adjsum [vecidx space (x,yb) | x<-([xa..xb])] - 2

distances galaxies space = (sum[distance a b space| a<-galaxies, b<-galaxies, a /= b])

parseFile filename = do
  contents <- readFile filename
  return (let space = processedGalaxies ([i|i<-(strSplit "\n" contents), length i > 0])
	in
	distances (findGalaxies space ) space)
