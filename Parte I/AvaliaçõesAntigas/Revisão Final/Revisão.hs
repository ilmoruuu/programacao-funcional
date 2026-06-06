module Revisão where

-- Simulado 2026

-- Questão 01
removeMin :: Ord a => [a] -> [a]
removeMin [] = []
removeMin xs = identifica ++ drop (length identifica + 1) xs
            where
                identifica = takeWhile (/= (minimum xs)) xs

-- Questão 02
escolheFuncoes :: [(a -> Bool)] -> a -> [(a-> Bool)]
escolheFuncoes [] _ = []
escolheFuncoes xs arg = [y | y <- xs, not (y arg)]

-- Questão 3
combinarCom :: (a -> b -> c) -> [a] -> [b] -> [c]
combinarCom _ [] _ = []
combinarCom _ _ [] = []
combinarCom func (x:xs) (y:ys) = func x y : combinarCom func xs ys

-- Questão 4
diferencaEntreListas :: Eq a => [a] -> [a] -> [a]
diferencaEntreListas l [] = l
diferencaEntreListas [] _ = []
diferencaEntreListas xs ys = [y | y <- xs, y `notElem` ys]

-- Avaliação I - 2025

-- Questão 1

-- Questão 2
dupli :: [a] -> [a]
dupli [] = []
dupli (x:xs) = x : x : dupli xs

-- Questão 3
pedaco :: [a] -> Int -> Int -> [a]
pedaco [] _ _ = []
pedaco xs a b = drop a (take (b+1) xs)

-- Questão 4
mapUm :: (a->b) -> [a] -> [b]
mapUm _ [] = []
mapUm func xs = [func y | y <- xs]

mapDois :: (a->b) -> [a] -> [b]
mapDois _ [] = []
mapDois func (x:xs) = func x : mapDois func xs

-- Avaliação I - 2024

-- Questão 1 
reverso :: [a] -> [a]
reverso [] = []
reverso xs = xs !! (length xs - 1) : reverso (init xs)

-- Questão 2
converter :: Int -> String
converter x
        | hrs < 10 && mins <10 && segs <10 = "0" ++ show hrs ++ ":0" ++ show mins ++ ":0" ++ show segs
        | hrs < 10 && mins <10 = "0" ++ show hrs ++ ":0" ++ show mins ++ ":" ++ show segs
        | hrs < 10 = "0" ++ show hrs ++ ":" ++ show mins ++ ":" ++ show segs
        | mins < 10 && segs <10 = "00:0" ++ show mins ++ ":0" ++ show segs
        | mins < 10 = "00:0" ++ show mins ++ ":" ++ show segs
        | segs < 10 = "00:00:0" ++ show segs
        | otherwise = show hrs ++ ":" ++ show mins ++ ":" ++ show segs
        where
            hrs = x `div` 3600 
            mins = (x`mod`3600) `div` 60
            segs = (x`mod`3600) `mod` 60


-- Questão 3
rotateLeft :: Int -> [a] -> [a]
rotateLeft _ [] = []
rotateLeft 0 l = l 
rotateLeft 1 (x:xs) = xs ++ [x]
rotateLeft vzs xs = rotateLeft (vzs-1) xs

-- Avaliação I - 2023

-- Questão 1 
dependentes :: [(String,[String])] -> [String]
dependentes [] = []
dependentes (x:xs) = snd x ++ dependentes xs


-- Questão 2
removeMin' :: Ord a => [a] -> [a]
removeMin' [] = []
removeMin' xs = identifica ++ drop (length identifica + 1) xs
            where
                identifica = takeWhile (/= (minimum xs)) xs


converterMelhorado :: Int -> String
converterMelhorado x = verifica hrs ++ ":" ++ verifica min ++ ":" ++ verifica segs   
                    where
                        hrs = x `div` 3600
                        min = (x`mod`3600) `div` 60
                        segs = (x`mod`3600) `mod` 60

verifica :: Int -> String
verifica x
        | x < 0 = error "Hora negativa!"
        | x < 10 = "0" ++ show x
        | otherwise = show x