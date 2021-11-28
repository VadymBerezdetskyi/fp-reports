
factorial :: Double -> Double
factorial 0 = 1
factorial n = n * factorial (n - 1)

ucosTailor :: Double -> Double -> Double -> Double
ucosTailor _ 0 prev = prev
ucosTailor x n prev = ucosTailor x (n - 1) (prev + cur)
  where cur = (((-1)**n) / factorial (2*n)) * x**(2*n)

uCos :: Double -> Double -> Double -> Double
uCos x n precision = do
  if next - current <= precision then next else uCos x (n + 1) 0
  where current = ucosTailor x n 0
        next = ucosTailor x (n + 1) 0

y :: Double -> Double
y x
  | x >= -1 && x <= 0 = uCos (x / 2) 0 0.0001 / uCos (x ** 2) 0 0.0001
  | x > 0 = (uCos (x / 2) 0 0.0001 ** 2) * uCos (2 * x) 0 0.0001
  | otherwise = -11111111111

firstTask = do 
    putStrLn "The error is:"
    let custom = uCos (-2) 0 0.00000000001
    let buildin = cos (-2)
    print (custom)
    print (buildin)
    print (abs (custom - buildin))

    -- putStrLn "The result is:"
    -- let res = [x | x <- [-2, -1.5 .. 2]]
    -- print (map y [-2, -1.5 .. 2])

fraction :: Double -> Double
fraction 0 = 1
fraction n = (2*n - 1) + (1 / fraction  (n - 1))

secondTask = do
  putStrLn "The result is:"
  print (fraction 3)