-- Wyznaczanie kodu Prüfera
-- https://pl.wikipedia.org/wiki/Kod_Pr%C3%BCfera


type Edge = (Int, Int)
type Vertex = Int

-- Główna funkcja do obliczania kodu Prüfera
pruferCode :: [Edge] -> [Vertex]
pruferCode edges
    | length vertices <= 2 = []
    | otherwise = pruferCode' vertices edges
  where
    vertices = removeDuplicates $ concatMap (\(x, y) -> [x, y]) edges

-- Usuwanie duplikatów z listy
removeDuplicates :: Eq a => [a] -> [a]
removeDuplicates []     = []
removeDuplicates (x:xs) = x : removeDuplicates (filter (/= x) xs)

-- Rekurencyjna funkcja do generowania kodu Prüfera
pruferCode' :: [Vertex] -> [Edge] -> [Vertex]
pruferCode' vertices edges
    | length vertices <= 2 = []
    | otherwise = parent : pruferCode' (filter (/= leaf) vertices) remainingEdges
  where
    leaf = findLeaf vertices edges
    parent = findParent leaf edges
    remainingEdges = filter (\(x, y) -> x /= leaf && y /= leaf) edges

-- Znajdowanie liścia (wierzchołka o stopniu 1)
findLeaf :: [Vertex] -> [Edge] -> Vertex
findLeaf vertices edges = head $ filter (\v -> countEdges v edges == 1) vertices

-- Znajdowanie rodzica liścia
findParent :: Vertex -> [Edge] -> Vertex
findParent leaf edges = case filter (\(x, y) -> x == leaf || y == leaf) edges of
    [(x, y)] -> if x == leaf then y else x
    _ -> error "Leaf without parent"

-- Liczenie krawędzi wychodzących z wierzchołka
countEdges :: Vertex -> [Edge] -> Int
countEdges v = length . filter (\(x, y) -> x == v || y == v)

main :: IO ()
main = do
    print $ pruferCode [(1, 2)]
    print $ pruferCode [(1, 2), (1, 3)]
    print $ pruferCode [(1, 2), (2, 3)]
    print $ pruferCode [(1, 3), (2, 3)]
    print $ pruferCode [(1, 4), (2, 4), (3, 4)]

    -- print $ pruferCode [(2, 1)]

