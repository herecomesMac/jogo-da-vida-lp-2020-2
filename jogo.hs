import System.IO
import Control.Monad

initial = ["v","m","m","z","m","z","m","m","m","z","v","v","m","v","m","m"]
linesamount = 4
columnsamount = 4

first (a,_,_) = a
second (_,b,_) = b
third (_,_,c) = c

-- Dado uma lista de índices, contar quantos são vivos, mortos e zumbis
count n [] (a,b,c) = (a,b,c)
count n indexes (a,b,c)
    | i < 0 || j < 0 = count n body (a, b, c)
    | i >= linesamount || j >= columnsamount = count n body (a, b, c)
    | n!!index == "m" = count n body (a, b + 1, c)
    | n!!index == "v" = count n body (a + 1, b, c)
    | n!!index == "z" = count n body (a, b, c + 1)
    where body = tail indexes
          i = head indexes !! 0
          j = head indexes !! 1
          index = i * columnsamount + j

-- Dado um indice (i,j) listar todos os adjacentes
getindexes i j = [[i-1,j+1], [i,j+1], [i+1,j+1],[i-1,j], [i+1,j],[i-1,j-1],[i, j-1], [i+1,j-1]] 

-- Função auxiliar para buscar os índices e contar os estados
adz n i = count n (getindexes (div i columnsamount) (mod i columnsamount)) (0,0,0)

-- Classifica uma posição como viva, morta ou zumbi
aliveordead n i 
   | n!!i == "m" && a == 3 = "v"
   | n!!i == "v" && z >= 1 = "z"
   | n!!i == "v" && a < 2  = "m"
   | n!!i == "v" && a > 3  = "m"
   | n!!i == "z" && a == 0 = "m"
   | otherwise = n!!i
   where a = first (adz n i)
         d = second (adz n i)
         z = third (adz n i)

-- Percorre todos as posições da tabela passada, classificando cada posição e adicionando na lista 'b'
travel n b i 
   | i >= linesamount * columnsamount = b
   | otherwise = travel n c (i + 1)
   where c = (aliveordead n i):b

-- Quantas vezes percorrer toda a tabela
-- Funcao checa se: tabela atual == tabela passada
--   Se sim: parar e retornar qtd de rodadas ate o momento e tabela atual
--   Se não: continuar execução
--   Se chegar ao fim do numero total de iteracoes sem estabilizar: printa estado final da tabela

gamertime n i total
  | i > 0 && m /= n = gamertime m (i - 1) total
  | i > 0 && m == n = print (total - i)
  | otherwise = print $ n 
  where m = reverse (travel n [] 0)

-- n é a quantidade de vezes que vai rodar
inicio n = do
   gamertime initial n n


main = do 
   inicio 2