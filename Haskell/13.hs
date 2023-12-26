-- Funkcja konwertująca ułamek na reprezentację binarną z okresem
toBinaryWithPeriod :: Int -> (String, Int)
toBinaryWithPeriod n = toBinaryWithPeriod' 1 n []

-- Pomocnicza funkcja rekurencyjna
toBinaryWithPeriod' :: Int -> Int -> [Int] -> (String, Int)
toBinaryWithPeriod' numerator denominator history
  | remainder == 0 = (binary, 0) -- Brak okresu
  | remainder `elem` history = (binary, length history - length (takeWhile (/= remainder) history))
  | otherwise = toBinaryWithPeriod' (remainder * 2) denominator (history ++ [remainder])
  where
    remainder = numerator `mod` denominator
    binary = if numerator >= denominator then "1" else "0"

-- Funkcja znajdująca najdłuższy okres
findMaxPeriod :: [(Int, String, Int)] -> Int
findMaxPeriod = maximum . map (\(_, _, period) -> period)

-- Funkcja znajdująca ułamek z najdłuższym okresem
longestBinaryPeriod :: Int -> [(Int, String, Int)]
longestBinaryPeriod n = filter (\(_, _, period) -> period == maxPeriod) binaryPeriods
  where
    binaryPeriods = [(i, binary, period) | i <- [2 .. n], let (binary, period) = toBinaryWithPeriod i]
    maxPeriod = findMaxPeriod binaryPeriods

-- Przykładowe wywołanie
main :: IO ()
main = print $ longestBinaryPeriod 10
