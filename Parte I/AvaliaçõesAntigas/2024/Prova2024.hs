module Prova2024 where

-- Questão 1 (2,5 pontos)
reverso :: [a] -> [a]
reverso [] = []
reverso xs = xs !! (length xs - 1) : reverso (init xs)


-- Questão 2 (2,5 Pontos)
converter :: Int -> String
converter 0 = "00:00:00"
converter s = show horas ++ ":" ++ show min ++ ":" ++  show seg 
            where
                horas = s`div`3600
                min = (s`mod` 3600) `div` 60
                seg = (s`mod`3600) `mod` 60

-- Questão 3 (2,5 Pontos)
rotateLeft :: Int -> [a] -> [a]
rotateLeft _ [] = []
rotateLeft 0 xs = xs
rotateLeft n xs = rotateLeft (n-1) (tail xs ++ [head xs])

-- Questão 4 (2,5 Pontos)
removeMin :: [Int] -> [Int]
removeMin [] = []
removeMin xs = identifica ++ drop (length identifica +1) xs
            where identifica = takeWhile ( /= minimum xs) xs