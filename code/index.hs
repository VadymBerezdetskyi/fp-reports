import Data.List
import System.IO
import Distribution.Compat.CharParsing (integral)

func :: Integral p => p -> p
func 0 = 0
func n = (n `mod` 10) + func (n `div` 10)

firstTask = do
    putStrLn "Input the number"
    input <- getLine
    let a = read input :: Integer
    print (func a)


deposit :: (Eq t1, Num t1, Fractional t2) => t2 -> t2 -> t1 -> t2
deposit sum _ 0 = sum
deposit sum percents periods = deposit (sum + (sum * percents/100)) percents (periods - 1)

secondTask = do
    putStrLn "Input the sum"
    inputSum <- getLine
    let sum = read inputSum :: Double

    putStrLn "Input the percents"
    inputPercents <- getLine
    let percents = read inputPercents :: Double
    
    putStrLn "Input the periods number"
    inputPeriods <- getLine
    let periods = read inputPeriods :: Integer

    print (deposit sum percents periods)