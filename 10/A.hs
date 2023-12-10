import qualified Data.Text as T
import Prelude as P
import System.IO
import qualified Data.List

strSplit :: [Char] -> [Char] -> [[Char]]
strSplit separator string = P.map T.unpack (T.splitOn (T.pack separator) (T.pack string))

digits = ['0'..'9']

-- coordinate system like svg: top left corner = 0,0

pipedir :: (Int, Int) -> Char -> (Int, Int)
pipedir (xPrev, yPrev) char = case char of
  '|' -> (0, yPrev)
  '-' -> (xPrev, 0)
  'L' -> if xPrev == -1 then (0,-1) else (1,0)
  'J' -> if xPrev == 1 then (0,-1) else (-1,0)
  '7' -> if xPrev == 1 then (0,1) else (-1,0)
  'F' -> if xPrev == -1 then (0,1) else (1,0)

vecadd (xa,ya) (xb, yb) = (xa + xb, ya + yb)

vecidx arr (x,y) = (arr!!y)!!x

startposition arr = head [(x,y) | y<-[0..(length arr - 1)], x<-[0..(length (arr!!0)-1)], vecidx arr (x,y) == 'S']

initialDirection = (1,0)

move :: ((Int, Int) , (Int, Int)) -> [String] -> Int -> Int
move (position, direction) pipes steps = let nextTilePos = position `vecadd` direction
  in
  let nextTileChar = vecidx pipes nextTilePos
  in
  if nextTileChar == 'S' then steps + 1 else
  move (nextTilePos, pipedir direction nextTileChar) pipes steps+1

parseFile filename = do
  contents <- readFile filename
  return (let arr = [line|line<-(strSplit "\n" contents),length line > 0]
	in
	move (startposition arr, initialDirection) arr 0

	)

