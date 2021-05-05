import System.IO
import Control.Monad

first (a,_,_) = a
second (_,b,_) = b
third (_,_,c) = c

-- Dado uma lista de índices, contar quantos são vivos, mortos e zumbis
count n linhas colunas [] (a,b,c) = (a,b,c)
count n linhas colunas indexes (a,b,c)
    | i < 0 || j < 0 = count n linhas colunas body (a, b, c)
    | i >= linhas || j >= colunas = count n linhas colunas body (a, b, c)
    | n!!index == "m" = count n linhas colunas body (a, b + 1, c)
    | n!!index == "v" = count n linhas colunas body (a + 1, b, c)
    | n!!index == "z" = count n linhas colunas body (a, b, c + 1)
    where body = tail indexes
          i = head indexes !! 0
          j = head indexes !! 1
          index = i * colunas + j

-- Dado um indice (i,j) listar todos os adjacentes
getindexes i j = [[i-1,j+1], [i,j+1], [i+1,j+1],[i-1,j], [i+1,j],[i-1,j-1],[i, j-1], [i+1,j-1]] 

-- Função auxiliar para buscar os índices e contar os estados
adz n linhas colunas i = count n linhas colunas (getindexes (div i colunas) (mod i colunas)) (0,0,0)

-- Classifica uma posição como viva, morta ou zumbi
aliveordead n linhas colunas i 
   | n!!i == "m" && a == 3 = "v"
   | n!!i == "v" && z >= 1 = "z"
   | n!!i == "v" && a < 2  = "m"
   | n!!i == "v" && a > 3  = "m"
   | n!!i == "z" && a == 0 = "m"
   | otherwise = n!!i
   where a = first (adz n linhas colunas i)
         d = second (adz n linhas colunas i)
         z = third (adz n linhas colunas i)

-- Percorre todos as posições da tabela passada, classificando cada posição e adicionando na lista 'b'
travel n linhas colunas b i 
   | i >= linhas * colunas = b
   | otherwise = travel n linhas colunas c (i + 1)
   where c = (aliveordead n linhas colunas i):b


-- Quantas vezes percorrer toda a tabela e funcao checa se: tabela atual == tabela passada (OUTPUT)
--   Se sim: parar e retornar qtd de rodadas ate o momento e tabela atual
--   Se não: continuar execução
--   Se chegar ao fim do numero total de iteracoes sem estabilizar: retorna estado final da tabela

gamertime n linhas colunas i total
  | i > 0 && m /= n = gamertime m linhas colunas (i - 1) total
  | i > 0 && m == n = print (total - i)
  | otherwise = print $ n 
  where m = reverse (travel n linhas colunas [] 0)

-- n é a quantidade de vezes que vai rodar
inicio n tabela linhas colunas = do
   gamertime tabela linhas colunas n n



-- INPUT


-- le arquivo e mapeia os elementos do arquivo que estão organizados em:

-- 1a linha: total de iteracoes
-- 2a linha: dimensao de linhas da matriz
-- 3a linha: dimensao de colunas da matriz
-- linhas em diante: estado inicial do jogo

readMatriz :: FilePath -> IO (Int , Int, Int, [String])
readMatriz file = fmap (parseLinhas . words) (readFile file)


readInt :: String -> Int
readInt = read


-- Divide array de string lido do arquivo em 4 elementos:

-- total de iteracoes
-- dimensao de linhas da matriz
-- dimensao de colunas da matriz
-- estado inicial do jogo

parseLinhas :: [String] -> (Int , Int, Int, [String])
parseLinhas (iteracoes : linhas : colunas : matriz ) = 
    (x, l, c, m)
    where 
        x = readInt iteracoes
        l = readInt linhas
        c = readInt colunas
        m = matriz

main = do
   putStrLn "\nteste 1:"
   (n1, l1, c1, jogo1) <- readMatriz "teste.txt"
   inicio n1 jogo1 l1 c1
   putStrLn "\nteste 2:"
   (n2, l2, c2, jogo2) <- readMatriz "teste2.txt"
   inicio n2 jogo2 l2 c2
   putStrLn "\nteste 3:"
   (n3, l3, c3, jogo3) <- readMatriz "teste3.txt"
   inicio n3 jogo3 l3 c3
   putStrLn "\nteste 4:"
   (n4, l4, c4, jogo4) <- readMatriz "teste4.txt"
   inicio n4 jogo4 l4 c4
   putStrLn "\nteste 5:"
   (n5, l5, c5, jogo5) <- readMatriz "teste5.txt"
   inicio n5 jogo5 l5 c5