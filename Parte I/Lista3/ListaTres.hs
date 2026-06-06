module ListaTres where

-- Questão 1
fatorial :: Int -> Int
fatorial 0 = 1
fatorial x = x * fatorial (x - 1)

fatorial2 :: Int -> Int
fatorial2 0 = 1
fatorial2 x 
        | x > 0 = x * fatorial2 (x-1)
        | otherwise = -x * fatorial2 (x+1)

-- Questão 2
somar :: Int -> Int
somar 0 = 0
somar x 
        | x > 0 = x + somar (x-1)
        | otherwise = -x + somar (x+1)


-- Questão 3
elevado :: Int -> Int -> Int
_`elevado` 0 = 1
x `elevado` y = product (replicate y x)

-- Questão 4
euclides :: Int -> Int -> Int
euclides x 0 = x
euclides x y
            | (y `mod` x ) == 0 = x
            | otherwise = y `euclides` (x `mod` y)


-- Questão 5

-- a)
meuAnd :: [Bool] -> Bool
meuAnd [] = True
meuAnd (x:xs) = x && and xs

-- b)
meuConcat :: [[a]] -> [a]
meuConcat [] = []
meuConcat (x:xs) = x ++ meuConcat xs


-- c) 
nésimo :: [a] -> Int -> a
nésimo xs 0 = head xs
nésimo xs p = nésimo (tail xs) (p-1)


-- d)
elem' :: Eq a => a -> [a] -> Bool
elem' _ [] = False
elem' a (x:xs)
        | a == x = True
        | otherwise = elem' a xs


-- Questão 6
merge :: Ord a => [a] -> [a] -> [a]
merge [] ys = ys
merge xs [] = xs
merge (x:xs) (y:ys)
                | x > y = y : merge (x:xs) ys
                | otherwise = x : merge xs (y:ys)

-- Questão 7
metade :: [a] -> ([a],[a])
metade [] = ([],[])
metade xs = (take ((length xs)`div`2) xs, drop ((length xs)`div`2) xs)


mergeSort :: Ord a => [a] -> [a]
mergeSort [] = []
mergeSort [x] = [x]
mergeSort xs = merge (mergeSort (fst (metade xs))) (mergeSort (snd (metade xs)))

-- Questão 8

-- a)
somaLista :: [Int] -> Int
somaLista [] = 0
somaLista (x:xs) = x + somaLista xs

-- b)
tamanhoLista :: [a] -> Int
tamanhoLista [] = 0 
tamanhoLista (x:xs) = 1 + tamanhoLista xs

-- c)
ultimo :: [a] -> a
ultimo [x] = x
ultimo (x:xs) = ultimo xs 
