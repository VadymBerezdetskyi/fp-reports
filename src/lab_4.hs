import Data.List
import System.IO
import Distribution.Compat.CharParsing (integral)

createList :: Int -> [Int]
createList a = [x * 3 | x <- [1..a]]

insertIntoList :: [Int] -> Int -> [Int] -> [Int]
insertIntoList el index list = left ++ el ++ right
  where (left, right) = splitAt index list

countEven :: [Int] -> Int
countEven list = length [x | x <- list, even x]

getMean :: [Int] -> Int
getMean list = ceiling (fromIntegral (sum list) / fromIntegral (length list))

replaceEvenWithMean:: [Int] -> [Int]
replaceEvenWithMean list = [ if even x then getMean list else x | x <- list]

firstTask = do
    putStrLn "Input length of list"
    input <- getLine
    let length = read input :: Int
    let list = createList length
    putStrLn "Result"
    print list
    
    putStrLn "List after inserting elements 1, 2, 3 index 4"
    print (insertIntoList [1, 2, 3] 4 list)

    putStrLn "Even numbers quantity in list:"
    print (countEven list)

    putStrLn "After replace all even elements by mean:"
    print (replaceEvenWithMean list)



