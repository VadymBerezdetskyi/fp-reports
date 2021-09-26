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


deposit sum _ 0 = sum
deposit sum percents periods = deposit (sum + (sum * percents/100)) percents (periods - 1)