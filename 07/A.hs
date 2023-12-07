import qualified Data.Text as T
import Prelude as P
import System.IO
import qualified Data.List

strSplit :: [Char] -> [Char] -> [[Char]]
strSplit separator string = P.map T.unpack (T.splitOn (T.pack separator) (T.pack string))

digits = ['0'..'9']

cards = "AKQJT98765432"

parseHand hand = [
  sum [1 | a<-hand, a == c] | c<-cards
  ]

safeIdx elem list = let tmp = Data.List.elemIndex elem list
  in
  case tmp of
  Just i -> i;
  Nothing -> 0



typeHand hand = let parsed = parseHand hand
  in
  if maximum parsed == 5 then 6 else
  if maximum parsed == 4 then 5 else
  if maximum parsed == 3 && 2 `elem` parsed then 4 else
  if maximum parsed == 3 then 3 else
  if sum [1|i<-parsed, i==2] == 2 then 2 else
  if maximum parsed == 2 then 1 else
  0

handToInt :: [Char] -> Int
handToInt (c:rest) = if length rest == 0 then (length cards) - safeIdx c cards - 1 else
  (length cards)*(handToInt rest) + handToInt [c]

greaterThanHands h1 h2 =
  if typeHand h1 > typeHand h2 then GT else
  if typeHand h2 > typeHand h1 then LT else
  if handToInt (reverse h1) > handToInt (reverse h2) then GT else LT
  
parseFile filename = do
  contents <- readFile filename
  return (let (hands:bets:_) = Data.List.transpose [ strSplit " " line | line<-(lines contents), length line > 0 ]
	in 
	let sortedHands = Data.List.sortBy greaterThanHands hands
	in sum [
	(read (bets !! i)::Int) * (safeIdx (hands !! i) sortedHands + 1)
	|
	i <- [0..(length bets - 1)]
	]

	)
