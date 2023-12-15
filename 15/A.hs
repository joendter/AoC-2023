import qualified Data.Text as T
import Prelude as P
import System.IO
import qualified Data.List
import qualified Data.Char

strSplit :: [Char] -> [Char] -> [[Char]]
strSplit separator string = P.map T.unpack (T.splitOn (T.pack separator) (T.pack string))

digits = ['0'..'9']

hash (a:string) = if string == "" then ((Data.Char.ord a)*17 `mod` 256) else
    ((hash string + Data.Char.ord a)*17 `mod` 256)

parseFile filename = do
    contents <- readFile filename
    print (sum [ hash (reverse i) |i<-(strSplit "," [c|c<-contents, c /= '\n']) , i /= ""])
