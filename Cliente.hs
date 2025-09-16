module Cliente where 

data Cliente = OrgGov String 
             | Empresa String Integer String String 
             | Individuo Pessoa Bool 
             deriving Show 

data Pessoa = Pessoa String String Genero
            deriving Show 

data Genero = Masculino 
            | Feminino 
            | Outro 
            deriving Show 


nomeCliente :: Cliente -> String 
nomeCliente (OrgGov nome) = nome 
nomeCliente (Empresa nome _ _ _) = nome 
nomeCliente (Individuo (Pessoa nome _ Masculino) _) = "Mr." ++ nome 
nomeCliente (Individuo (Pessoa nome _ Feminino) _) = "Ms." ++ nome 

data Expr = BTrue 
          | BFalse 
          | Num Int 
          | Add Expr Expr 
          | And Expr Expr 
          deriving Show 

step :: Expr -> Expr 
step (Add (Num n1) (Num n2)) = Num (n1 + n2)
step (Add (Num n1) e) = Add (Num n1) (step e)
step (Add e1 e2) = Add (step e1) e2 

primeiroTupla3 :: (a, b, c) -> a 
primeiroTupla3 (v,_,_) = v

segundoTupla3 :: (a, b, c) -> b 
segundoTupla3 (_, v, _) = v 

terceiroTupla3 :: (a, b, c) -> c 
terceiroTupla3 (_, _, v) = v 

listaEhVazia :: [a] -> Bool 
listaEhVazia [] = True 
listaEhVazia _ = False 

cabeca :: [a] -> a 
cabeca (h:_) = h 

cauda :: [a] -> [a] 
cauda (_:t) = t 

segundoLista :: [a] -> a 
segundoLista (_:x:_) = x 

fatorial :: Integer -> Integer 
fatorial 0 = 1 
fatorial n = fatorial (n - 1) * n 
