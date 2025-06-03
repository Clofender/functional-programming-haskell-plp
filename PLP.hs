-- Nome: Daniel Silva Ferraz Neto, Matrícula: 202410191
-- Nome: Luis Kennedy Gervasio Turola, Matrícula:
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