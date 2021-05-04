import System.IO
import Control.Monad


main = do
    contents <- readFile "teste.txt"
    print . map readInt . words $ contents


readLines :: FilePath -> IO [String]
readLines = fmap lines . readFile
makeInteger :: [String] -> [Int]
makeInteger = map read

main = do
  content <- readLines "teste.txt"
  print (radix $ makeInteger content)

readInt :: String -> Int
readInt = read