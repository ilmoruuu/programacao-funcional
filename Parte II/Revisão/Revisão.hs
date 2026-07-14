module Revisão where
import Data.Array (Ix(range))

-- Questão 2
newtype Real' = RS Double
newtype Dolar = US Double deriving Show

converter :: Double -> Real' -> Dolar
converter cam (RS valor) = US (valor / cam)

-- Questão 3

data Arvore a = Folha a | No (Arvore a) a (Arvore a)

altura :: Arvore a -> Int
altura (Folha _) = 1
altura (No esq a dir)
                    | x > y = x
                    | otherwise = y
                    where
                        x = 1 + altura esq
                        y = 1 + altura dir

-- Questão 4
contarFolhas :: Arvore a -> Int
contarFolhas (Folha _) = 1
contarFolhas (No esq a dir) = contarFolhas esq + contarFolhas dir

-- Questão 5
data Naipe = Copas | Espadas | Ouros | Paus

instance Eq Naipe where
    Copas == Copas = True
    Espadas == Espadas = True
    Ouros == Ouros = True
    Paus == Paus = True
    _ == _ = False


-- Questão 6

data Prioridade = Baixa | Media | Alta deriving (Eq, Show)

instance Ord Prioridade where
    Baixa < Media = True
    Media < Alta  = True
    Baixa < Alta  = True
    _ < _         = False
    a <= b = (a < b) || a == b -- O Ord cobra as instâncias de (<=)

-- Questão 7
ecoChar :: IO()
ecoChar = do
    putStrLn "Digite uma letra"
    letra <- getChar
    putChar letra 

-- Questão 8
imprimirString :: String -> IO()
imprimirString [] = return()
imprimirString (x:xs) = do
                    putChar x
                    imprimirString xs

-- Questão 9
contarEntrada :: IO()
contarEntrada = do
            putStrLn "Digite uma linha"
            line <- getLine
            let range = length line
            putStrLn ("Você digitou" ++ show range ++ "Caracteres")


-- Questão 10
menu :: IO ()
menu = do
    putStrLn "1-Saudação"
    putStrLn "2-Despedida"
    putStrLn "3-Sair"
    
    opcao <- getChar
    _     <- getChar 

    if opcao == '1' then do
        putStrLn "Olá, Mundo!"
        menu 
    else if opcao == '2' then do
        putStrLn "Até logo!"
        menu 
    else if opcao == '3' then
        return ()  
    else do
        putStrLn "Opção inválida!"
        menu 
