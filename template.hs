import qualified Data.Text as T
import Prelude as P
import System.IO
import qualified Data.List

strSplit :: [Char] -> [Char] -> [[Char]]
strSplit separator string = P.map T.unpack (T.splitOn (T.pack separator) (T.pack string))

digits = ['0'..'9']

