filterIndex :: Eq b => [b] -> ([b] -> b) -> Int
filterIndex xs predicate = head $ filter ((== predicate xs) . (xs !!)) [0..]

firstTask = do
  let v = [10, 2, 4, 5, 9]

  print ("Maximum element is " ++ show (maximum v))
  print ("Maximum element index is " ++ show (filterIndex v maximum))

  print ("Minimum element is " ++ show (minimum v))
  print ("Minimum element index is " ++ show (filterIndex v minimum))

showReverse :: Integral a => [a] -> a -> [a]
showReverse stack m = filter (\x -> x `mod` m /= 0) (reverse stack)

secondTask = do
  let stack = [10, 9 .. 1]
  putStrLn "Stack:"
  print stack

  putStrLn "Input the number"
  input <- getLine
  let m = read input :: Int

  putStrLn "Reverse stack:"
  print (showReverse stack m)