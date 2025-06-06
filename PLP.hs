-- Nome: Daniel Silva Ferraz Neto, Matrícula: 202410191
-- Nome: Luis Kennedy Gervasio Turola, Matrícula: 202410840
-- Grupo: Grupo 2 - Funções 2, 5, 8, 11, 14, 17, 20, 23, 26, 29, 32, 35 e 38.

module TrabalhoFuncional where

-- Funcao 2
-- insere_no_fim: recebe um elemento e uma lista e insere o elemento no final da lista.
-- ex: insere_no_fim 3 [1,2] -> [1,2,3]
insere_no_fim :: t -> [t] -> [t]
insere_no_fim elemento [] = [elemento]
insere_no_fim elemento (cabeca:resto) = cabeca : insere_no_fim elemento resto


-- Funcao 5
-- concatena: Une duas listas colocando os elementos da primeira no início e os da segunda no final.
-- ex: concatena [1,2] [3,4] -> [1,2,3,4]
concatena :: [t] -> [t] -> [t]
concatena [] resto2 = resto2
concatena (cabeca1:resto1) resto2 = cabeca1 : concatena resto1 resto2

-- Funcao 8
-- remover_repetidos: recebe uma lista e retorna outra lista sem repetição de elementos.
-- A primeira ocorrência de cada elemento é mantida, e a ordem original dos elementos preservada.
-- ex.: remover_repetidos [7,4,3,5,7,4,4,6,4,1,2] -> [7,4,3,5,6,1,2]
remover_repetidos :: Eq a => [a] -> [a]
remover_repetidos [] = []
remover_repetidos (x:xs) = x : remover_repetidos (filter (/= x) xs)

-- Funcao 11
-- variacoes: recebe uma lista de números e retorna outra,
-- com as variações (diferenças) de cada valor para o próximo.
-- A lista resultante terá um elemento a menos que a lista original.
-- ex.: variacoes [1,3,3,7,3,0] -> [2,0,4,-4,-3]
variacoes :: Num a => [a] -> [a]
variacoes [] = []       -- Nenhuma variação para lista vazia.
variacoes [_] = []      -- Nenhuma variação para lista com um único elemento.
variacoes (x1:x2:xs) = (x2 - x1) : variacoes (x2:xs)


-- FUNCAO 14
sequencia :: Integral t=> t -> t -> [t]
sequencia 0 _ = []
sequencia n m = m : sequencia (n-1) (m+1)

-- FUNCAO 17
--17 Uniao
uniao :: Eq t => [t] -> [t] -> [t]
uniao plist [] = plist --plist = primeira lista
uniao plist (i:slist)  --slist = segunda lista
	| contem plist i = uniao plist slist
	| otherwise = uniao (plist ++ [i]) slist
	where
		contem [] _ = False
		contem (a:as)b
			| a == b = True
			| otherwise = contem as b


-- FUNCAO 20
insere_ordenado :: Ord t => t -> [t] -> [t]
insere_ordenado elemento [] = [elemento]
insere_ordenado elemento (cabeca:resto)
	| elemento <= cabeca	= elemento : cabeca : resto
	| otherwise	= cabeca : insere_ordenado elemento resto

-- Função auxiliar para 'mediana' (Função 23), não faz parte da lista de entrega direta.
-- ordena: ordena uma lista usando o princípio de insertion sort, utilizando a 'insere_ordenado' já definida.
ordena :: Ord a => [a] -> [a]
ordena [] = []
ordena (x:xs) = insere_ordenado x (ordena xs)

-- FUNCAO 23
-- mediana: recebe uma lista de números e retorna a mediana deles.
-- ex.: mediana [6,2,9,0,8,3,0,2] -> 2.5
mediana :: (Ord a, Real a, Fractional b) => [a] -> b
mediana [] = error "mediana: lista vazia nao tem mediana definida."
mediana l =
    let sorted_l = ordena l -- Usa a função 'ordena' definida acima
        n = length sorted_l
        mid = n `div` 2
    in if odd n
       then fromRational (toRational (sorted_l !! mid)) -- Elemento central para tamanho ímpar
       else (fromRational (toRational (sorted_l !! (mid - 1))) + fromRational (toRational (sorted_l !! mid))) / 2.0 -- Média dos dois centrais para tamanho par


--FUNCAO 26
seleciona_ultimo :: [t] -> ([t], t)
seleciona_ultimo [elemento] = ([], elemento)  -- Se tiver só um elemento
seleciona_ultimo (cabeca:resto) = (cabeca:inicio, ultimo)
	where (inicio, ultimo) = seleciona_ultimo resto

rotaciona_ultimo :: [t] -> [t]
rotaciona_ultimo [] = []
rotaciona_ultimo lista = ultimo : inicio
	  where (inicio, ultimo) = seleciona_ultimo lista


rodar_direita :: Int -> [t] -> [t]
rodar_direita 0 lista = lista                          -- 0 rotações, devolve lista original
rodar_direita _ [] = []                          -- lista vazia
rodar_direita n lista = rodar_direita (n-1) (rotaciona_ultimo lista)


--FUNCAO 29
soma_conta :: [Float] -> (Float, Int)
soma_conta [] = (0,0)
soma_conta (cabeca : resto) = (cabeca + somatorio, 1 + contador)
	where (somatorio, contador) = soma_conta resto

media :: [Float] -> Float
media lista = soma / fromIntegral contador
	where (soma, contador) = soma_conta lista

--FUNCAO 32
seleciona :: [t] -> [Int] -> [t]
seleciona _ [] = []
seleciona lista (cabeca : resto) = elemento_posicao lista (cabeca - 1) : seleciona lista resto
  where
    elemento_posicao :: [t] -> Int -> t
    elemento_posicao (cabeca : _) 0 = cabeca
    elemento_posicao (_ : resto) n = elemento_posicao resto (n - 1)

--FUNCAO 35
primo :: Int -> Bool
primo n
  | n <= 1    = False
  | otherwise = verifica_divisor (n - 1)
  where
    verifica_divisor :: Int -> Bool
    verifica_divisor 1 = True
    verifica_divisor divisor
      | n `mod` divisor == 0 = False
      | otherwise = verifica_divisor (divisor - 1)

-- Função auxiliar para 'compactar' (Função 38).
-- agrupa_elementos_consecutivos: Agrupa elementos iguais e consecutivos em sublistas.
-- Ex: agrupa_elementos_consecutivos [1,1,1,2,2,3,1] -> [[1,1,1],[2,2],[3],[1]]
agrupa_elementos_consecutivos :: Eq a => [a] -> [[a]]
agrupa_elementos_consecutivos [] = []
agrupa_elementos_consecutivos (x:xs) = (x : takeWhile (==x) xs) : agrupa_elementos_consecutivos (dropWhile (==x) xs)

-- Função auxiliar para 'compactar' (Função 38).
-- formata_grupo_compactado: Formata um grupo de elementos (uma sublista da saída de agrupa_elementos_consecutivos).
formata_grupo_compactado :: (Eq a, Num a) => [a] -> [a]
formata_grupo_compactado [] = error "formata_grupo_compactado: grupo vazio nao deveria ocorrer"
formata_grupo_compactado grupo@(g:_) =
    let count = length grupo
    in if count > 1
       then [fromIntegral count, g] -- fromIntegral para converter Int (de length) para Num a
       else [g]

-- FUNCAO 38
-- compactar: recebe uma lista de inteiros e transforma repetições consecutivas.
-- O resultado é uma lista de listas.
-- ex.: compactar [2,2,2,3,4,4,2,9,5,2,4,5,5,5] -> [[3,2],[3],[2,4],[2],[9],[5],[2],[4],[3,5]]
compactar :: (Eq a, Num a) => [a] -> [[a]]
compactar [] = []
compactar xs = map formata_grupo_compactado (agrupa_elementos_consecutivos xs)
