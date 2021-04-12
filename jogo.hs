initial = [1,0,0,2,0,2,0,0,0,2,1,1,0,1,0,0]
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
    | n!!index == 0 = count n body (a, b + 1, c)
    | n!!index == 1 = count n body (a + 1, b, c)
    | n!!index == 2 = count n body (a, b, c + 1)
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
   | n!!i == 0 && a == 3 = 1
   | n!!i == 1 && z >= 1 = 2
   | n!!i == 1 && a < 2  = 0
   | n!!i == 1 && a > 3  = 0
   | n!!i == 2 && a == 0 = 0
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
gamertime n i 
  | i > 0 = gamertime m (i - 1)
  | otherwise = n
  where m = reverse (travel n [] 0)


-- n é a quantidade de vezes que vai rodar
inicio n = do
   gamertime initial n


