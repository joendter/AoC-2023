import Data.Text as T
import Prelude as P
import System.IO

strSplit :: [Char] -> [Char] -> [[Char]]
strSplit separator string = P.map T.unpack (T.splitOn (T.pack separator) (T.pack string))

digits = ['0'..'9']

tuplemax :: (Integer, Integer, Integer) -> (Integer, Integer, Integer) -> (Integer, Integer, Integer)
tuplemax (r1,g1,b1) (r2,g2,b2) = (max r1 r2, max g1 g2, max b1 b2)

gamePower :: (Integer, Integer, Integer) -> Integer
gamePower (r,g,b) = r*g*b


parseOne :: String -> (Integer, Integer, Integer)
parseOne string = 
    let n = read [d | d <- string, d `elem` digits] :: Integer
        color = [l | l <- string, l `elem` ['a'..'z']]
    in 
        (
            if (color == "red"   ) then n else 0,
            if (color == "green"   ) then n else 0,
            if (color == "blue"   ) then n else 0
        )

parseHand :: String -> (Integer, Integer, Integer)
parseHand string = P.foldl tuplemax (0,0,0) (P.map parseOne (strSplit "," string))

parseGame string = 
    P.foldl tuplemax (0,0,0) (P.map parseHand (strSplit ";" (P.last (strSplit ":" string))))



parseFile filename = do
    contents <- readFile filename
    return (sum [ gamePower a|a <- (P.map parseGame (P.lines contents))])
