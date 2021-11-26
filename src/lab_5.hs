asRational :: (Show a1, Show a2) => (a1, a2) -> [Char]
asRational rat = show (fst rat) ++ "/" ++ show (snd rat)

ratLcm :: Integral a => (a, a) -> (a, a) -> a
ratLcm rat1 rat2 = z `div` gcd (b*d) z
  where a = fst rat1
        b = snd rat1
        d = snd rat2
        z = lcm (a*d) (b * fst rat2) 

firstTask = do
  let fRat = (1, 2)
  let sRat = (3, 4)
  let res = "Least common multiple of " ++ asRational fRat ++ " and " ++ asRational sRat ++ " is " ++ show (ratLcm fRat sRat)
  putStrLn res

getR :: Floating a => (a, a) -> a
getR com = sqrt (fst com ** 2 + snd com ** 2)

getPhi :: Floating a => (a, a) -> a
getPhi com = acos (fst com / getR com)

asTrygonometric :: (Show a, Floating a) => (a, a) -> [Char]
asTrygonometric com = "z = " ++ show (getR com) ++ "(cos(" ++ phi ++ ") + isin(" ++ phi ++ "))"
  where phi = show (getPhi com)
  
asAgebraic :: (Show a1, Show a2) => (a1, a2) -> String 
asAgebraic com = "z = " ++ show (fst com) ++ " + " ++ show (snd com) ++ "i"

secondTask = do
  let complex = [(1, 3), (4, 3)]
  putStrLn "Numbers in algebraic form"
  putStrLn (unlines [asAgebraic x | x <- complex])

  putStrLn "Numbers in trygonometric form"
  putStrLn (unlines [asTrygonometric x | x <- complex])


  