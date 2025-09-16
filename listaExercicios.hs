import Text.Read (Lexeme(String))
import Data.Char (toLower)
import GHC.Exts.Heap (GenClosure(fun))

-- Exercicio 1



salarioLiquido:: Float -> Float
salarioLiquido salarioBase  = salarioBase + ((salarioBase * 0.1) - (salarioBase * 0.07))
                    
-- Exercicio 2

-- Função foi passada como parâmetro da função imprimeConceito (Exemplo:1)  e chamada dentro dela (Exemplo:2) 
notaFinal:: Float -> Float -> Float -> Float
notaFinal n m p = ((n * 0.2) + (m * 0.3) + (p * 0.5))
 
-- Exemplo:1
--Função foi passada como parâmetro da função imprimeConceito
imprimeConceito:: (Float -> Float -> Float -> Float) -> Float -> Float -> Float -> String
imprimeConceito f n m p
    | ((media <= 10.0) && (media >= 8.0)) = imprime "A"
    | ((media < 8.0) && (media >= 7.0)) = imprime "B"
    | ((media < 7.0) && (media >= 6.0)) = imprime "C"
    | ((media < 6.0) && (media >= 5.0)) = imprime "D"
    | ((media < 5.0) && (media >= 0.0)) = imprime "E"
    | otherwise = "Nota invalida"
    where media = f n m p
          imprime conceito =  "Media: " ++ show media ++ ", " ++ "Conceito: " ++ conceito


-- Exemplo:2
-- Função não foi passada como parâmetro e sim chamada diretamente
-- imprimeConceito::Float -> Float -> Float -> String
-- imprimeConceito n m p
--     | ((media <= 10.0) && (media >= 8.0)) = imprime "A"
--     | ((media < 8.0) && (media >= 7.0)) = imprime "B"
--     | ((media < 7.0) && (media >= 6.0)) = imprime "C"
--     | ((media < 6.0) && (media >= 5.0)) = imprime "D"
--     | ((media < 5.0) && (media >= 0.0)) = imprime "E"
--     | otherwise = "Nota invalida"
--     where media = notaFinal n m p
--           imprime conceito =  "Media: " ++ show media ++ ", " ++ "Conceito: " ++ conceito

    -- Exercicio 3 
-- Haskell não converte Int para Float, Float para Double ou Double para Float automaticamente, 
-- então é necessário fazer a conversão manualmente usando as funções fromIntegral, realToFrac

-- individuos:: Int -> Float
-- individuos n 
--     | n == 1 = 100.0
--     | n == 2 = 130.0
--     | n == 3 = 150.0
--     | n == 4 = 165.0
--     | n == 5 = 175.0
--     | n == 6 = 180.0
--     | otherwise = 185.0

-- individuos:: Int -> Float
-- individuos 1 = 100.0
-- individuos 2 = 130.0
-- individuos 3 = 150.0
-- individuos 4 = 165.0
-- individuos 5 = 175.0
-- individuos 6 = 180.0
-- individuos _ = 185.0

-- precoRetrato :: Int -> String -> Double
-- precoRetrato n dia
--     | dia == "domingo" || dia == "sabado" = preco * 1.2
--     | otherwise                           = preco
--   where
--     preco = realToFrac (individuos n) :: Double

individuos:: Int -> Double
individuos 1 = 100.0
individuos 2 = 130.0
individuos 3 = 150.0
individuos 4 = 165.0
individuos 5 = 175.0
individuos 6 = 180.0
individuos _ = 185.0


-- map toLower dia converte a string dia para minúsculas, para isso é necessário importar o módulo Data.Char (import Data.Char (toLower))
precoRetrato :: Int -> String -> Double
precoRetrato n dia
    | map toLower dia == "domingo" || map toLower dia == "sabado" = preco * 1.2
    | otherwise                           = preco
  where
    preco = individuos n


-- Exercicio 4

fatorialDuplo:: Int -> Int
fatorialDuplo 2 = 2
fatorialDuplo 1 = 1
fatorialDuplo n = n * fatorialDuplo (n - 2)
    

-- Exercicio 5
calcularPotencia:: Int -> Int -> Int
calcularPotencia n 0 = 1
calcularPotencia n m = n * calcularPotencia n (m - 1)


-- Exerccicio 6
salarioAtual :: Float -> Int -> Int -> Float
salarioAtual salarioInicial anoContratacao anoAtual
    | anoAtual <= anoContratacao = salarioInicial
    | otherwise = salarioInicial * (1 + aumentoAno anoServico)
  where
    -- calcula o percentual de aumento para o ano atual
    anoServico = anoAtual - anoContratacao
    aumentoAno ano
        | ano  == 1 = 0.015           -- primeiro aumento
        | otherwise       = 2 * aumentoAno (ano - 1)

-- Exercicio 7

primeiroLista:: [a] -> a
primeiroLista (h:_) = h   

segundoLista:: [a] -> a
segundoLista (_:m:_) = m 

ultimo:: [a] -> a
ultimo [x] = x  
ultimo (_:t) = ultimo t

-- ultimo:: [a] -> a
-- ultimo (_:x) = last x

-- usando recursão para achar o elemento do meio e a função init para remover o último elemento da lista
-- meioLista:: [a] -> a
-- meioLista [x] = x
-- meioLista [x,y] = y
-- meioLista (_:t) = meioLista (init t)    

-- !! para pegar o elemento na posição n da lista, começando do 0
meioLista:: [a] -> a
meioLista x = x !! (length x `div` 2)

-- Exercicio 8
primeiros:: [a] -> [a]
primeiros [] = []
primeiros t = init t

-- exercicio 9
-- multElementoListas:: Num a => [a] -> [a] -> [a]
-- multElementoListas [] _ = []
-- multElementoListas _ [] = []
-- multElementoListas n m = zipWith (*) n m 

multElementoListas :: Num a => [a] -> [a] -> [a]
multElementoListas [] _ = []
multElementoListas _ [] = []
multElementoListas (x:xs) (y:ys) = (x * y) : multElementoListas xs ys

-- Exercicio 10
type Codigo = Int
type Descricao = String
type AnoValidade = Int
type Comestivel = Bool
type Fabricante = String
type AnoFabricacao = Int

data Produto 
    = Perecivel         Codigo Descricao AnoValidade Comestivel  
    | NaoPerecivel      Codigo Descricao Fabricante AnoFabricacao Comercializacao
    deriving Show

    
-- produtoValido (Perecivel 1 "Milho pipoca" 2028 True) 2027

--Exercicio 11
data Comercializacao = Unidade Int
                    | Peso Double
                    deriving Show

produtoComercializadoPor :: Produto -> String
produtoComercializadoPor (NaoPerecivel _ _ _ _ comercializa) = 
    case comercializa of
        Unidade n -> show n ++ " unidades"
        Peso p    -> show p ++ " kg"    
-- produtoComercializadoPor (NaoPerecivel 1 "Milho pipoca" "Teste" 2028 (Unidade 8) )  > "8 unidades"



-- Exercicio 12
produtoValido :: Produto -> Int -> Bool
produtoValido (Perecivel _ _ anoValidade _) anoAtual 
    | anoAtual <= anoValidade = True
    | otherwise               = False
produtoValido (NaoPerecivel _ _ _ anoFabricacao comercializa) anoAtual = True


-- produtoValido (Perecivel 1 "Milho pipoca" 2028 True) 2027  > True
-- produtoValido (Perecivel 1 "Milho pipoca" 2028 True) 2029  > False
-- produtoValido (NaoPerecivel 1 "Milho pipoca" "Teste" 2028 (Unidade 8) ) 2029  > True 
-- produtoValido (NaoPerecivel 1 "Milho pipoca" "Teste" 2028 (Peso 2.5) ) 2029  > True

-- Exercicio 13
funcAnd:: Bool -> Bool -> Bool
funcAnd True True = True
funcAnd _ _ = False

funcOr:: Bool -> Bool -> Bool
funcOr False False = False      
funcOr _ _ = True

-- Exercicio 14
listaNumeros:: Num a => [a] -> a
listaNumeros [] = 0
listaNumeros [x]
        |length [x] == 1 = x
listaNumeros (x:y:_) = x + y


