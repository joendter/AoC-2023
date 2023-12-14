import qualified Data.Text as T
import Prelude as P
import System.IO
import qualified Data.List

strSplit :: [Char] -> [Char] -> [[Char]]
strSplit separator string = P.map T.unpack (T.splitOn (T.pack separator) (T.pack string))

digits = ['0'..'9']

vecadd (xa,ya) (xb, yb) = (xa + xb, ya + yb)

vecneg (x,y) = (-x,-y)

vecidx arr (x,y) = if 0 <= x && x <= (length(head arr)-1) && 0 <= y && y <= length arr -1 then (arr!!y)!!x
    else '#'

fullyRolled board direction = not (foldl (||) False [
    vecidx board (x,y) == 'O' && vecidx board (vecadd (x,y) direction) == '.'
    |
    y<-[0..(length board -1)],
    x<-[0..(length (head board)-1)]
    ])

rollDirection :: [[Char]] -> (Int, Int) -> [[Char]]
rollDirection board direction = 
    let next = [        [        if vecidx board (x,y) == '.' && vecidx board (vecadd (x,y) (vecneg direction)) == 'O' then 'O' else         if vecidx board (x,y) == 'O' && vecidx board (vecadd (x,y) direction) == '.' then '.' else        vecidx board (x,y)        |        x<-[0..(length (head board)-1)]        ]        |        y<-[0..(length board -1)]        ] 
    in
    if fullyRolled next direction then next else
    rollDirection next direction

score board = let (l, w) = (length board, length (head board))
    in
    sum [ l-y
    |
    y <- [0..(l-1)],
    x <- [0..(w-1)],
    vecidx board (x,y) == 'O'
    ]

parseFile filename = do
    contents <- readFile filename
    return (score (rollDirection [l|l<-(strSplit "\n" contents), length l > 0] (0,-1)))

