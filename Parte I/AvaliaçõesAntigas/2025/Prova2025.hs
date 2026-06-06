module Prova2025 where

-- Questão 1 (3,5 pontos)
fatoresPrimo :: Int -> [(Int, Int)]
fatoresPrimo 0 = [(0,0)]
fatoresPrimo n = [(a,sum (fator a n))| a <- [2..n], n`mod`a == 0, ehPrimo a == True]

fator :: Int -> Int -> [Int]
fator 0 _ = error "0 não pode ser divisor"
fator f n 
    | n `mod`f /= 0 = []
    | otherwise =  1 : fator f (n `div` f)

ehPrimo :: Int -> Bool
ehPrimo x
        | (length teste) == 2 = True
        | otherwise = False
        where teste = [y | y <- [1..x], x`mod`y == 0]
 
-- Questão 2 (2,5 pontos)
dupli :: [a] -> [a]
dupli [] = []
dupli (x:xs) = x : x : dupli xs

-- Questão 3 (2,5 pontos)
pedaco :: [a] -> Int -> Int -> [a]
pedaco [] _ _ = []
pedaco xs a b = drop a (take (b+1) xs)

-- Questão 4 (1,5 pontos)
mapi :: (a->b) -> [a] -> [b]
mapi _ [] = []
mapi fun (x:xs) = [fun y | y <- (x:xs)]