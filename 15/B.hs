import qualified Data.Text as T
import Prelude as P
import System.IO
import qualified Data.List
import qualified Data.Char

strSplit :: [Char] -> [Char] -> [[Char]]
strSplit separator string = P.map T.unpack (T.splitOn (T.pack separator) (T.pack string))


type Box = [(String, Int)]

hash (string) =
    let (a, rest) = (last string, init string)
    in
    if rest == "" then ((Data.Char.ord a)*17 `mod` 256) else
    ((hash rest + Data.Char.ord a)*17 `mod` 256)

startBoxes = take 256 (repeat [])

boxElem :: String -> Box -> Bool
boxElem label boxes= length [ 1| (a,_)<-boxes, a == label ] > 0

process :: [String] -> [Box] -> [Box]
process instructions boxes = 
    if length instructions == 0 then boxes else
    let (current, rest) = (head instructions, tail instructions)
    in
    let chrs = [ c| c<-current, Data.Char.isLetter c]
    in
    let box = hash chrs
    in
    let operator = [c | c<-current, c `elem` "-=" ]
    in
    let focal = read [c | c<-current, Data.Char.isDigit c] :: Int
    in
    if operator == "=" then
        process rest [ 
        if i /= box then boxes !! i else
        if not (chrs `boxElem` (boxes !! i)) then
        boxes !! i ++ [(chrs, focal)]
        else
            [ 
            if label == chrs then (label, focal) else
            (label, foc)
            | (label, foc)<-(boxes!!i) ]
        | i <- [0..(length boxes - 1)] ]
    else
        process rest [
        if i /= box then boxes !! i else
        [ lens | lens<-(boxes!!i), fst lens /= chrs ]
        | i <- [0..(length boxes - 1)] ]

focusingPower :: [Box] -> Int
focusingPower boxes = sum
    [
    (i+1) * (sum [ (j+1)*(snd $(boxes !! i) !! j)| j<-[0..(length (boxes !! i) -1)] ])
    |
    i <- [0..(length boxes -1)
    ]]

parseFile filename = do
    contents <- readFile filename
    print $ focusingPower (process (strSplit "," (contents )) startBoxes)
