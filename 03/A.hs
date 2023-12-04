import System.IO
import Data.List

symbolBitmap :: [[Char]] -> [[Bool]]
symbolBitmap stringArray= [[not (char `elem` (['0'..'9'] ++ ['.']))| char <- line] | line<- stringArray]


numberBitmap :: [[Char]] -> [[Bool]]
numberBitmap stringArray= [[char `elem` ['0'..'9'] | char <- line] | line<- stringArray]


parseLine previous current next = 
  let tPrev = "." ++ previous ++ "." ++ (take (length previous) (repeat '.'))
  in
  let tCurr = "." ++ current ++ "." ++ (take (length current) (repeat '.'))
  in
  let tNext = "." ++ next ++ "." ++ (take (length next) (repeat '.'))
  in
  sum [if (isNumberRelevant tPrev tCurr tNext sel) then getNum tCurr sel else 0| sel<-(splitStrNum (reverse tCurr)), snd sel > 0]

isNumberRelevant previous current next (start, delta)= let 
    stuff = take (fromInteger delta + 2) 
                (drop (fromInteger start - 1)
                        (transpose 
                            (symbolBitmap [previous, current, next])))
    in foldl (||) False (map (foldl (||) False) stuff)


getFile filename = do
    contents <- readFile filename
    return (let liness = [repeat '.'] ++  (lines contents) ++ [repeat '.']
            in (sum [parseLine (liness !!(i-1)) (liness !!(i)) (liness !!(i+1))| i<-[1..(length liness -2)]]))
  

-- tupleTransform :: (Integer, Bool) -> (Integer, Bool) -> (Integer, Bool)
tupleTransform (a,b) (c,d) = (a + if b then c else 0, b&&d)

-- isNumber :: Char -> (Integer, Bool)
isNumber char = let n = char `elem` ['0'..'9']
				in (if n then 1 else 0, n)

isNonNumber :: Char -> (Integer, Bool)
isNonNumber char = let n = not (char `elem` ['0'..'9'])
				in (if n then 1 else 0, n)



lenOfNum string = fst (foldl tupleTransform (0,True) (map isNumber string))
lenOfNonNum string = fst (foldl tupleTransform (0,True) (map isNonNumber string))

splitStrNum string = if string == "" then [(-4,0)] else 
                                                  let nonNum= lenOfNonNum string 
												  in
												  let num = lenOfNum (drop (fromInteger nonNum) string)
												  in
												  let rest = (splitStrNum (drop (fromInteger (nonNum + num)) string))
												  in
												  let (l3, l4) = last rest
												  in
												  let totalOffset = l3 + l4 + 1 
                                                  in rest ++ [(nonNum+totalOffset, num)] 

getNum :: [Char] -> (Integer, Integer) -> Integer
getNum line (start, delta) = read (take (fromInteger delta) (drop (fromInteger start) line)) :: Integer
