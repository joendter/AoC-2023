import Control.Monad (replicateM)
import System.IO
import Text.Regex.Posix
import Data.List (isSuffixOf, isPrefixOf)

readLines n f = withFile f ReadMode $ replicateM n . hGetLine

digitsFromString s = [c | c <- s , c `elem` ['0'..'9']]
calibrationFromString s = read (take 1 (digitsFromString s) ++ [last (digitsFromString s)]) :: Integer
parseLines lines = sum [ calibrationFromString l | l<-lines ]

findFirstLastMatch :: String -> String -> Maybe (String, String)
findFirstLastMatch regex str = do
    let firstMatch = getAllTextMatches (str =~ regex) :: [String]
    return (head firstMatch, last firstMatch)

certain stri = case stri of
	Just x -> x
	Nothing -> ("0","0")

parseString :: String -> Integer
parseString string = 
  if length string == 1
	then read string :: Integer
	else 
	case string of
	  "one" -> 1
	  "oneight" -> 1
	  "two" -> 2
	  "twone" -> 2
	  "three" ->3 
	  "threeight" -> 3
	  "four" -> 4
	  "five" -> 5
	  "fiveight" -> 5
	  "six" -> 6
	  "seven" -> 7
	  "eight" -> 8
	  "nine" -> 9
	  "nineight"-> 9
	  _ -> 0

parseMatches :: (String, String) -> Integer
parseMatches (a,b) = (parseString a)*10 + if (last b) == 't' then 8 else if ("one" `isSuffixOf` b)
then 1
else (parseString b)

parseLines' lines = sum [
  parseMatches (certain (findFirstLastMatch regex_cursed line)) | line <- lines
  ]


noRegex :: String -> Integer
noRegex stri = let  first = head [j | j <-[prefixcase (drop i stri) | i <- [0..(length stri - 1)]], j>0]
                    lastw = head [j | j <-[suffixcase (take i stri) | i <- reverse [1..(length stri)] ], j>0]
  in 10*first + lastw 


suffixcase :: String -> Integer
suffixcase stri = if last stri `elem` ['1'..'9'] then read [(last stri)] :: Integer else
  if "one"	  `isSuffixOf` stri then 1 else
  if "two"	  `isSuffixOf` stri then 2 else
  if "three"  `isSuffixOf` stri then 3 else
  if "four"	  `isSuffixOf` stri then 4 else
  if "five"	  `isSuffixOf` stri then 5 else
  if "six"	  `isSuffixOf` stri then 6 else
  if "seven"  `isSuffixOf` stri then 7 else
  if "eight"  `isSuffixOf` stri then 8 else
  if "nine"	  `isSuffixOf` stri then 9 else
  0

prefixcase :: String -> Integer
prefixcase stri = if head stri `elem` ['1'..'9'] then read [(head stri)] :: Integer else
  if "one"	  `isPrefixOf` stri then 1 else
  if "two"	  `isPrefixOf` stri then 2 else
  if "three"  `isPrefixOf` stri then 3 else
  if "four"	  `isPrefixOf` stri then 4 else
  if "five"	  `isPrefixOf` stri then 5 else
  if "six"	  `isPrefixOf` stri then 6 else
  if "seven"  `isPrefixOf` stri then 7 else
  if "eight"  `isPrefixOf` stri then 8 else
  if "nine"	  `isPrefixOf` stri then 9 else
  0 

parseLinesNoRegex lines = sum [
	noRegex line | line <- lines
  ]


regex_cursed = "[1-9]|one(ight)?|two(ne(ight)?)?|three(ight)?|four|five(ight)?|six|seven|eight|nine(ight)?"

main' nlines filename = do
  linesOfFile <- readLines nlines filename
  return (parseLinesNoRegex linesOfFile)
