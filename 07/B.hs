import qualified Data.Text as T
import Prelude as P
import System.IO
import qualified Data.List

strSplit :: [Char] -> [Char] -> [[Char]]
strSplit separator string = P.map T.unpack (T.splitOn (T.pack separator) (T.pack string))

digits = ['0'..'9']

cards = "AKQT98765432J"

parseHand hand = let tmp = [sum [1 | a<-hand, a == c] | c<-cards]
  in (init tmp, last tmp)

safeIdx elem list = let tmp = Data.List.elemIndex elem list
  in
  case tmp of
  Just i -> i
  Nothing -> 0


-- 6 = penta, 5 = quad, 4 = full house, 3 = triple, 2 = two pair, 1 = pair, 0 = high card
typeHand hand = let (parsed, jokers) = parseHand hand
  in
  if maximum parsed == 5 - jokers then 6 else
  if maximum parsed == 4 - jokers then 5 else
  if jokers == 2 then 3 else
  if jokers == 1 then ( 
	if sum [1|i<-parsed, i==2] == 2 then 4 else
    if maximum parsed == 2 then 3 else 1)
  else (
	if maximum parsed == 3 && 2 `elem` parsed then 4 else
    if maximum parsed == 3 then 3 else
    if sum [1|i<-parsed, i==2] == 2 then 2 else
    if maximum parsed == 2 - jokers then 1 else 0)

handToInt :: [Char] -> Int
handToInt (c:rest) = if length rest == 0 then (length cards) - safeIdx c cards - 1 else (length cards)*(handToInt rest) + handToInt [c]

greaterThanHands h1 h2 =
  if typeHand h1 > typeHand h2 then GT else
  if typeHand h2 > typeHand h1 then LT else
  if handToInt (reverse h1) > handToInt (reverse h2) then GT else LT
  
parseFile filename = do
  contents <- readFile filename
  return (let (hands:bets:_) = Data.List.transpose [ strSplit " " line | line<-(lines contents), length line > 0 ]
	in 
	let sortedHands = Data.List.sortBy greaterThanHands hands
	in (sum [(read (bets !! i)::Int) * (safeIdx (hands !! i) sortedHands + 1) |	i <- [0..(length bets - 1)]	]))
