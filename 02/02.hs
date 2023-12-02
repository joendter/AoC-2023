import Data.Text as T
import Prelude as P
import System.IO


redLimit = 12
greenLimit = 13
blueLimit = 14

strSplit :: [Char] -> [Char] -> [[Char]]
strSplit separator string = P.map T.unpack (T.splitOn (T.pack separator) (T.pack string))

digits = ['0'..'9']

parseOne :: [Char] -> Bool
parseOne string = 
    let n = read [d | d <- string, d `elem` digits] :: Integer
        color = [l | l <- string, l `elem` ['a'..'z']]
    in 
        ((color == "red"   ) <= (n <= redLimit)) &&
        ((color == "green" ) <= (n <= greenLimit)) &&
        ((color == "blue"  ) <= (n <= blueLimit))
 
parseHand :: [Char] -> Bool
parseHand string = let
    logic = P.map parseOne (strSplit "," string)
    in P.foldl (&&) True logic


parseGame string = let
    gamenumber = read [ d|d <- P.head (strSplit ":" string), d `elem` digits] :: Integer
    validity = P.foldl (&&) True (P.map parseHand (strSplit ";" (P.last (strSplit ":" string))))
    in
        (gamenumber, validity)

parseFile filename = do
    contents <- readFile filename
    return (sum [ number|(number, valid) <- (P.map parseGame (P.lines contents)), valid])
