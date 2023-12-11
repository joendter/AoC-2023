import qualified Data.Text as T
import Prelude as P
import System.IO
import qualified Data.List as L

strSplit :: [Char] -> [Char] -> [[Char]]
strSplit separator string = P.map T.unpack (T.splitOn (T.pack separator) (T.pack string))

digits = ['0'..'9']

galaxyExpansionHelper :: [String] -> String -> [String]
galaxyExpansionHelper prevGalaxies galaxy = if sum [1|i<-galaxy, i == '#'] > 0 then prevGalaxies ++ [galaxy] else prevGalaxies ++ [galaxy, galaxy]

processedGalaxies galaxies = let horizontal = foldl galaxyExpansionHelper [] galaxies
  in
  foldl galaxyExpansionHelper [] (L.transpose horizontal)

vecadd (xa,ya) (xb, yb) = (xa + xb, ya + yb)

vecidx arr (x,y) = (arr!!y)!!x


findGalaxies space = [(x,y)| y<- [0..(length space - 1)], x<- [0..(length (space!!0)-1)], vecidx space (x,y) == '#']

distance (xa,ya) (xb,yb) = abs (xa-xb) + abs (ya-yb)

distances galaxies = (sum[distance a b| a<-galaxies, b<-galaxies, a /= b])

parseFile filename = do
  contents <- readFile filename
  return (distances (findGalaxies (processedGalaxies ([i|i<-(strSplit "\n" contents), length i > 0]))))
