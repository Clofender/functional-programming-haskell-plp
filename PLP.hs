-- Nome: Daniel Silva Ferraz Neto, Matrícula: 202410191
-- Nome: Luis Kennedy Gervasio Turola, Matrícula: 202410840
-- Grupo: Grupo 2 - Funções 2, 5, 8, 11, 14, 17, 20, 23, 26, 29, 32, 35 e 38.

module TrabalhoFuncional where

-- Funcao 2
-- insere_no_fim: recebe um elemento e uma lista e insere o elemento no final da lista.
-- ex.: insere_no_fim 3 [1,2] -> [1,2,3]
insere_no_fim :: a -> [a] -> [a]
insere_no_fim el [] = [el]
insere_no_fim el (x:xs) = x : insere_no_fim el xs

-- Funcao 5
-- concatena: recebe duas listas quaisquer e retorna uma terceira lista
-- com os elementos da primeira no início e os elementos da segunda no fim.
-- ex.: concatena [1,2] [3,4] -> [1,2,3,4]
concatena :: [a] -> [a] -> [a]
concatena [] ys = ys
concatena (x:xs) ys = x : concatena xs ys

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
--Sequencia: recebe um valor n e um inicial m,
--retorna uma lista ta que [m,m+1,...] n vezes
--ex: sequencia 3 4 => [4,5,6] 
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
