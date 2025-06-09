-- Nome: Daniel Silva Ferraz Neto, Matrícula: 202410191
-- Nome: Luis Kennedy Gervasio Turola, Matrícula: 202410840
-- Grupo: Grupo 2 - Funções 2, 5, 8, 11, 14, 17, 20, 23, 26, 29, 32, 35 e 38.

module TrabalhoFuncional where
-- FUNCOES PRINCIPAIS

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

-- 20. insere_ordenado: insere elemento em lista ordenada (crescente)
insere_ordenado :: Ord t => t -> [t] -> [t]
insere_ordenado elemento [] = [elemento]
insere_ordenado elemento (cabeca:resto)
  | elemento <= cabeca = elemento : cabeca : resto
  | otherwise = cabeca : insere_ordenado elemento resto

-- Ordena usando insere_ordenado 
ordena :: Ord t => [t] -> [t]
ordena [] = []
ordena (cabeca:resto) = insere_ordenado cabeca (ordena resto)

-- 23. mediana: retorna a mediana da lista de números
mediana :: (Ord t, Real t, Fractional b) => [t] -> b
mediana l
  | odd n     = fromRational (toRational (sorted_l !! mid))
  | otherwise = (fromRational (toRational (sorted_l !! (mid - 1))) + fromRational (toRational (sorted_l !! mid))) / 2.0
  where
    sorted_l = ordena l
    n = length sorted_l
    mid = n `div` 2


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

-- 32. seleciona: recebe lista e lista de posições (1-based)
seleciona :: [t] -> [Int] -> [t]
seleciona _ [] = []
seleciona lista (cabeca:resto) = elemento_posicao lista (cabeca - 1) : seleciona lista resto
  where
    elemento_posicao (c:_) 0 = c
    elemento_posicao (_:r) n = elemento_posicao r (n - 1)
   
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


-- 38. compactar: agrupa repetições consecutivas em sublistas [quantidade, valor]
compactar :: (Eq a, Num a) => [a] -> [[a]]
compactar [] = []
compactar xs = map formata_grupo_compactado (agrupa_elementos_consecutivos xs)
  where
    formata_grupo_compactado grupo@(g:_)
      | length grupo > 1 = [fromIntegral (length grupo), g]
      | otherwise        = [g]

    agrupa_elementos_consecutivos :: Eq a => [a] -> [[a]]
    agrupa_elementos_consecutivos [] = []
    agrupa_elementos_consecutivos (x:xs) =
      (x : takeWhile (== x) xs) : agrupa_elementos_consecutivos (dropWhile (== x) xs)

