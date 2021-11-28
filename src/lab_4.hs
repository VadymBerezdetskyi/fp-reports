import Data.List (intercalate)
import System.IO ()
import Numeric (showInt)

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

showIdle :: Show a => a -> [Char]
showIdle idle = "The idle is " ++ show idle ++ " hours"

showContent :: (Num t, Enum t, Show a) => a -> t -> String
showContent carType qty = "Train: " ++ intercalate "-" [show carType | x <- [0..qty]]
showIntervals :: Show a => a -> String
showIntervals int = "The interval is " ++ show int ++ " hours"
showCouplingDuration :: Int -> Int -> String
showCouplingDuration carType qty = "Train coupling duration: " ++ show (qty * (dur !! carType)) ++ " hours"
  where dur = [1, 2]
showInspectionDuration :: Int -> Int -> String
showInspectionDuration carType qty = 
  "Train coupling duration: " ++ show (qty * (dur !! carType)) ++ " hours"
  where dur = [2, 1]

showTrain :: [Int] -> String
showTrain train = unlines [
    showContent (head train) (train !! 1),
    showIntervals (train !! 2),
    showCouplingDuration (head train) (train !! 1),
    showInspectionDuration (head train) (train !! 1),
    showIdle (train !! 3),
    "-------------"
  ]

secondTask = do
  -- (car type, cars quantity, interval, idle time)
  let trains = [[1, 7, 24, 3], [0, 8, 36, 2]]
  putStrLn (unlines [showTrain x | x <- trains])

