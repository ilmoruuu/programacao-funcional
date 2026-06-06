module Prova2023 where

-- Questão 1
depends :: [(String, [String])] -> [String]
depends [] = []
depends (x:xs) = snd x ++ depends xs

-- Questão 2

removeMin :: [Int] -> [Int]
removeMin [] = []
removeMin xs = verifica ++ drop (length verifica +1) xs
            where verifica = takeWhile (/= minimum xs) xs


-- Questão 3
converter :: Int -> String
converter 0 = "00:00:00"
converter x 
            | x < 0 = error "Valor de segundos negativo!"
            | otherwise = show horas ++ ":" ++ show min ++ ":" ++ show segs 
            where
                horas = x `div` 3600
                min = (x `mod` 3600) `div` 60
                segs = (x `mod` 3600) `mod` 60