import System.IO
import Data.List

symbolBitmap :: [[Char]] -> [[Bool]]
symbolBitmap stringArray= [[not (char `elem` (['0'..'9'] ++ ['.']))| char <- line] | line<- stringArray]


numberBitmap :: [[Char]] -> [[Bool]]
numberBitmap stringArray= [[char `elem` ['0'..'9'] | char <- line] | line<- stringArray]


parseLine previous current next = 0

isNumberRelevant previous current next start stop = let 
    stuff = fst 
            (splitAt (stop - start) 
                (snd 
                    (splitAt start 
                        (transpose 
                            (symbolBitmap [previous, current, next])))))
    in foldl (||) False (map (foldl (||) False) stuff)


getFile filename = do
    contents <- readFile filename
    return (numberBitmap (lines contents))