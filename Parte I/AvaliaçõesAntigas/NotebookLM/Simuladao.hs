module Simuladao where

-- Questão 1
aprovados :: [(String, [Float], Int)] -> [String]
aprovados xs = [nome | (nome, notas, faltas) <- xs, 
                       (sum notas / fromIntegral (length notas)) >= 7.0, 
                       faltas < 10]


-- Questão 2
formata :: Int -> String
formata x
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
pegaEnquanto :: (a-> Bool) -> [a] -> [a]
pegaEnquanto _ [] = []
pegaEnquanto fun (x:xs)
                    | fun x = x : pegaEnquanto fun xs
                    | otherwise = []

-- Questão 4
intercalar :: [a] -> [a] -> [a]
intercalar _ [] = []
intercalar [] _ = []
intercalar (x:xs) (y:ys) = x : y : intercalar xs ys