module Prova where


-- Questão 1
intercalar :: a -> [a] -> [a]
intercalar _ [] = []
intercalar _ [a] = [a]
intercalar e (x:xs) = x : e : intercalar e xs

-- Questão 2
frequencia :: Eq a => a -> [a] -> Int
frequencia _ [] = 0
frequencia e (x:xs)
                    | x == e = 1 + frequencia e xs
                    | otherwise = 0 + frequencia e xs


-- Questão 3
substituir :: Eq a => a -> a -> [a] -> [a]
substituir _ _ [] = []
substituir a n (x:xs)
                    | a == x = n : substituir a n xs
                    | otherwise = x : substituir a n xs

-- Questão 4
totalEstoque :: [(String, Float, Float)] -> Float
totalEstoque [] = 0.0 
totalEstoque xs = sum produto
            where
                produto = [qntd * valor | (nome, qntd, valor) <- xs]


-- Questão 5
quantidades :: Eq a => [a] -> [(a,Int)]
quantidades [] = []
quantidades xs = [(e, conta e xs) | e <- xs]

conta :: Eq a => a -> [a] -> Int
conta _ [] = 0
conta e (x:xs)
            | x == e = 1 + conta e xs
            | otherwise = 0 + conta e xs