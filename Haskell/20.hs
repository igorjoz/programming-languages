rotations :: Int -> [Int]
rotations n = map read (rotate (show n))
  where
    rotate xs = take (length xs) (iterate (\(y:ys) -> ys ++ [y]) xs)

isPrime :: Int -> Bool
isPrime n
  | n <= 1 = False
  | otherwise = all (\x -> n `mod` x /= 0) [2..limit]
  where
    limit = floor . sqrt $ fromIntegral n

primeRotations :: Int -> [Int]
primeRotations n = filter (\x -> all isPrime (rotations x)) [1..n]

main :: IO ()
main = do
  putStrLn "Podaj liczbę naturalną n:"
  input <- getLine
  let number = read input :: Int
  let result = primeRotations number
  putStrLn ("Liczby <= " ++ show number ++ ", których wszystkie rotacje są liczbami pierwszymi:")
  print result