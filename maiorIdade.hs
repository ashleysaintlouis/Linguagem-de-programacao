idade :: Int -- Um valor inteiro constante
idade = 17
maiorDeIdade :: Bool -- Usa a definição de idade
maiorDeIdade = (idade>=18)
quadrado :: Int -> Int -- função que eleva um número ao quadrado
quadrado x = x * x
mini :: Int -> Int -> Int -- função que mostra o menor valor entre dois inteiros
mini a b
        | a <= b = a
        | otherwise = b

ouEx:: Bool -> Bool -> Bool
ouEx x y = (x || y) && not(x && y)

type Nome = String
type Idade = Int

verificarIdade:: (Nome, Idade) -> Idade
verificarIdade (_,b) = b

aluno:: Int -> Float
aluno 1 = 7.5
aluno 2 = 8.0           
aluno 3 = 6.5
aluno 4 = 9.0
aluno 5 = 5.0
aluno 6 = 9.5



soma:: Int -> Float
soma 1 = aluno 1
soma n = aluno n + soma (n-1)

media:: Int -> Float
media n = (soma n) / (fromInteger (toInteger n))


tabela:: Int -> String
tabela n = cabecalho ++ imprimeAlunos n ++ imprimeMedia n

cabecalho:: String
cabecalho = "Aluno" ++ "                 " ++  "Nota"++"\n"

imprimeAlunos:: Int -> String
imprimeAlunos 1 = imprimeAluno 1
imprimeAlunos n = imprimeAlunos (n-1) ++ imprimeAluno n

imprimeAluno:: Int -> String
imprimeAluno n = show n ++ "                 " ++ show (aluno n) ++ "\n"

imprimeMedia:: Int -> String
imprimeMedia n = "\n" ++ "Média da turma: " ++ show (media n) ++ "\n"


somaPares:: [(Int,Int)] -> [Int]
somaPares lista = [(a+b) | (a,b) <- lista]

pares:: [t] -> [u] -> [(t,u)]
pares n m = [(a,b) | a <- n, b <- m] 

remove:: Char -> [Char] -> [Char]
remove carac str = [c | c <- str , c /= carac]

