-- Nome: Daniel Silva Ferraz Neto, Matrícula: 202410191
-- Nome: Luis Kennedy Gervasio Turola, Matrícula: 202410840
-- Grupo: Grupo 2 - Funções 2, 5, 8, 11, 14, 17, 20, 23, 26, 29, 32, 35 e 38.

module TrabalhoFuncional where
-- 2. insere_no_fim: insere um elemento no final da lista
insere_no_fim :: t -> [t] -> [t]
insere_no_fim elemento [] = [elemento]
insere_no_fim elemento (cabeca:resto) = cabeca : insere_no_fim elemento resto

-- 5. concatena: concatena duas listas
concatena :: [t] -> [t] -> [t]
concatena [] lista2 = lista2
concatena (cabeca1:resto1) lista2 = cabeca1 : concatena resto1 lista2


--- 8. remover_repetidos: remove elementos repetidos de uma lista
remover_repetidos :: Eq t => [t] -> [t]
remover_repetidos [] = []
remover_repetidos (cabeca:resto)
  | esta_na_lista cabeca resto = remover_repetidos resto
  | otherwise = cabeca : remover_repetidos resto
  where
    esta_na_lista :: Eq t => t -> [t] -> Bool
    esta_na_lista _ [] = False
    esta_na_lista alvo (cabeca:resto)
      | alvo == cabeca  = True
      | otherwise  = esta_na_lista alvo resto


-- 11. variacoes: lista das diferenças entre elementos consecutivos
variacoes :: Num t => [t] -> [t]
variacoes [] = []
variacoes [_] = []
variacoes (elemento1:elemento2:resto) = (elemento2 - elemento1) : variacoes (elemento2:resto)

-- 14. sequencia: gera sequência crescente de n elementos a partir de m
sequencia :: Integral t => t -> t -> [t]
sequencia 0 _ = []
sequencia n m = m : sequencia (n-1) (m+1)

-- 17. uniao: união de duas listas sem repetição
uniao :: Eq t => [t] -> [t] -> [t]
uniao lista1 lista2 = remover_repetidos (concatena lista1 lista2)

-- 20. insere_ordenado: insere elemento em lista ordenada
insere_ordenado :: Ord t => t -> [t] -> [t]
insere_ordenado elemento [] = [elemento]
insere_ordenado elemento (cabeca:resto)
  | elemento <= cabeca = elemento : cabeca : resto
  | otherwise = cabeca : insere_ordenado elemento resto

-- Ordena
ordena :: Ord t => [t] -> [t]
ordena [] = []
ordena (cabeca:resto) = insere_ordenado cabeca (ordena resto)

-- 23. mediana: retorna a mediana da lista de números
-- Número de elementos de uma lista
tamanho_lista :: [t] -> Int
tamanho_lista [] = 0
tamanho_lista (_:resto) = 1 + tamanho_lista resto

-- Acessa o elemento da lista na posição i (indexação 0-based)
elemento_na_posicao :: [t] -> Int -> t
elemento_na_posicao (cabeca:_) 0 = cabeca
elemento_na_posicao (_:resto) i = elemento_na_posicao resto (i - 1)

-- Mediana
mediana :: (Ord t, Real t, Fractional b) => [t] -> b
mediana lista
  | tamanho `mod` 2 == 1 = realToFrac (elemento_na_posicao lista_ordenada meio)
  | otherwise = (realToFrac (elemento_na_posicao lista_ordenada (meio - 1)) +
                 realToFrac (elemento_na_posicao lista_ordenada meio)) / 2.0
  where
    lista_ordenada = ordena lista
    tamanho = tamanho_lista lista_ordenada
    meio = tamanho `div` 2

-- 26. rodar_direita: "rola" a lista para a direita n vezes
rodar_direita :: Int -> [t] -> [t]
rodar_direita 0 lista = lista
rodar_direita _ [] = []
rodar_direita n lista = rodar_direita (n-1) (rotaciona_ultimo lista)
  where
    rotaciona_ultimo [] = []
    rotaciona_ultimo lista = ultimo : inicio
      where (inicio, ultimo) = seleciona_ultimo lista

    seleciona_ultimo [cabeca] = ([], cabeca)
    seleciona_ultimo (cabeca:resto) = (cabeca:inicio, ultimo)
      where (inicio, ultimo) = seleciona_ultimo resto

-- 29. media: retorna média aritmética de uma lista de Float
media :: [Float] -> Float
media lista = soma / fromIntegral cont
  where
    (soma, cont) = soma_conta lista

    soma_conta [] = (0, 0)
    soma_conta (cabeca:resto) = (cabeca + soma, 1 + cont)
      where (soma, cont) = soma_conta resto

-- 32. seleciona: recebe lista e lista de posições
seleciona :: [t] -> [Int] -> [t]
seleciona _ [] = []
seleciona lista (cabeca:resto) = elemento_na_posicao lista (cabeca - 1) : seleciona lista resto
   
-- 35. primo: verifica se um número é primo
primo :: Int -> Bool
primo numero
  | numero <= 1 = False
  | otherwise = verifica_divisor (numero - 1)
  where
    verifica_divisor 1 = True
    verifica_divisor d
      | numero `mod` d == 0 = False
      | otherwise      = verifica_divisor (d - 1)


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
