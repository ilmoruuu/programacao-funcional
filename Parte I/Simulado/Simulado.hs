module Simulado where


-- Questão 1
removeMin :: [Int] -> [Int]
removeMin [] = []
removeMin xs = takeWhile (>minimum xs) xs ++ dropWhile (<= minimum xs) xs

-- Questão 2 (Anulada)
-- escolheFuncoes :: [(a-> b)] -> a -> [(a-> b)]
-- escolheFuncoes [] _ = []
-- escolheFuncoes xs arg = [show y | y <- xs, (y arg) == False]

-- Questão 3 
combinarCom :: (a->b->c) -> [a] -> [b] -> [c]
combinarCom _ [a] [] = []
combinarCom _ [] [b] = []
combinarCom _ [] [] = []
combinarCom op (x:xs) (y:ys) = op x y : combinarCom op xs ys


-- Questão 4 
pertence :: Eq a => a -> [a] -> Bool
pertence _ [] = False
pertence v (x:xs)
        | v == x = True
        | otherwise = pertence v xs

diferencaEntreListas :: Eq a => [a] -> [a] -> [a]
diferencaEntreListas [a] [] = [a]
diferencaEntreListas [] [b] = [b]
diferencaEntreListas [] [] = []
diferencaEntreListas xs ys = [z | z <- xs, (z`pertence` ys) == False ]