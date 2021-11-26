import System.IO
import Data.Char (toLower)
import Data.List ( elemIndex )
import Data.Maybe ( fromMaybe, isNothing )
import Data.Text ()

indexOf :: Eq a => a -> [a] -> Int
indexOf el list
  | isNothing (elemIndex el list) = -1
  | otherwise = fromMaybe (-1) $ elemIndex el list

getIndexWithOffset :: Foldable t => Int -> t a -> Int
getIndexWithOffset idx list
  | idx < Prelude.length list = idx
  | otherwise =  idx - Prelude.length list

caesar :: [Char] -> Int -> [Char]
caesar str k = [
    if indexOf x alphabet == -1
      then x
      else alphabet !! getIndexWithOffset (indexOf x alphabet + k) alphabet
  | x <- str]
  where alphabet = "abcdefghijklmnopqrstuvwxyz"

encrypt :: String -> String
encrypt contents = Prelude.unlines [Prelude.unwords [if even (lIdx + 1) && even (wIdx + 1) then caesar word 4 else word | (wIdx, word) <- line] | (lIdx, line) <- cLines]
  where cLines = Prelude.zip [0..] [Prelude.zip [0..] (Prelude.words x) | x <- Prelude.lines contents]

make = do
  writeFile "test.txt" "Lorem Ipsum is asedasdasd dummy text of the printing and typesetting industry. \nLorem Ipsum has been the industry's standard dummy text ever since the 1500s, \nwhen an unknown printer took a galley of type and scrambled it to make a type specimen book.\nIt has survived not only five centuries, but also the leap into electronic"

  contents <- readFile "test.txt"
  putStr contents

  writeFile "testencrypted.txt" (encrypt (Prelude.map Data.Char.toLower contents))
  
  putStr "\n\nEncrypted file:\n"
  contents <- readFile "testencrypted.txt"
  putStr contents