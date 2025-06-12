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


-- 8. remover_repetidos: remove elementos repetidos de uma lista
--Mantém a última ocorrência do elemento na lista
remover_repetidos :: Eq t => [t] -> [t]
remover_repetidos [] = []
remover_repetidos (cabeca:resto)
  | esta_na_lista cabeca resto = remover_repetidos resto
  | otherwise = cabeca : remover_repetidos resto
  where
    esta_na_lista _ [] = False
    esta_na_lista alvo (cabeca:resto)
      | alvo == cabeca  = True
      | otherwise  = esta_na_lista alvo resto


-- 11. variacoes: lista das diferenças entre elementos consecutivos (i=(i+1)-i)
variacoes :: Num t => [t] -> [t]
variacoes [] = []
variacoes [_] = []
variacoes (cabeca1:cabeca2:resto) = (cabeca2 - cabeca1) : variacoes (cabeca2:resto)


-- 14. sequencia: gera sequência crescente de n elementos a partir de m
sequencia :: Integral t => t -> t -> [t]
sequencia 0 _ = []
sequencia n m = m : sequencia (n-1) (m+1)


-- 17. uniao: união de duas listas sem repetição
--Concatena duas listas e depois remove os elementos repetidos
uniao :: Eq t => [t] -> [t] -> [t]
uniao lista1 lista2 = remover_repetidos (concatena lista1 lista2) 


-- 20. insere_ordenado: insere elemento em uma lista ordenada
insere_ordenado :: Ord t => t -> [t] -> [t]
insere_ordenado elemento [] = [elemento]
insere_ordenado elemento (cabeca:resto)
  | elemento <= cabeca = elemento : cabeca : resto --Se o elemento for menor que a cabeça, vira a nova cabeça
  | otherwise = cabeca : insere_ordenado elemento resto


-- Ordena: Ordena uma lista usando insere_ordenado
ordena :: Ord t => [t] -> [t]
ordena [] = []
ordena (cabeca:resto) = insere_ordenado cabeca (ordena resto)


-- Tamanho da lista/ numero de elementos
tamanho_lista :: [t] -> Int
tamanho_lista [] = 0
tamanho_lista (_:resto) = 1 + tamanho_lista resto


-- Retorna o elemento de uma lista uma posição específica
elemento_na_posicao :: [t] -> Int -> t
elemento_na_posicao (cabeca:_) 0 = cabeca
elemento_na_posicao (_:resto) i = elemento_na_posicao resto (i - 1)


-- 23. mediana: retorna a mediana da lista numérica
mediana :: (Ord t, Fractional t) => [t] -> t
mediana lista
  | odd tamanho     = elemento_na_posicao listOrdenada meio
  | otherwise = (elemento_na_posicao listOrdenada (meio - 1) + elemento_na_posicao listOrdenada meio) / 2
  where
    listOrdenada = ordena lista
    tamanho = tamanho_lista listOrdenada
    meio = tamanho `div` 2


-- 26. rodar_direita: rotaciona a lista para a direita n vezes
rodar_direita :: Int -> [t] -> [t]
rodar_direita 0 lista = lista
rodar_direita _ [] = []
rodar_direita n lista = rodar_direita (n-1) (rotaciona_ultimo lista)
  where
    --Faz o último elemento do resto virar a cabeça
    rotaciona_ultimo [] = []
    rotaciona_ultimo lista = ultimo : inicio
      where (inicio, ultimo) = seleciona_ultimo lista
    
    seleciona_ultimo [cabeca] = ([], cabeca)
    seleciona_ultimo (cabeca:resto) = (cabeca:inicio, ultimo)
      where (inicio, ultimo) = seleciona_ultimo resto


-- 29. media: média aritmética de uma lista numérica
media :: [Float] -> Float
media lista = soma / fromIntegral cont
  where
    (soma, cont) = soma_conta lista

    soma_conta [] = (0, 0)
    soma_conta (cabeca:resto) = (cabeca + soma, 1 + cont)
      where (soma, cont) = soma_conta resto


-- 32. seleciona: Retorna uma lista de elementos selecionados pelo índice
seleciona :: [t] -> [Int] -> [t]
seleciona _ [] = []
seleciona lista (cabeca:resto) = elemento_na_posicao lista (cabeca - 1) : seleciona lista resto
   
   
-- 35. primo: verifica se um número é primo ou não
primo :: Int -> Bool
primo numero
  | numero <= 1 = False
  | otherwise = verifica_divisor (numero - 1)
  where
    verifica_divisor 1 = True
    verifica_divisor d
      | numero `mod` d == 0 = False
      | otherwise      = verifica_divisor (d - 1)
     
      
-- 38. compactar: Transforma sequências repetidas em [contagem, valor] e itens únicos em [valor]. 
compactar :: (Eq t, Num t) => [t] -> [[t]]
compactar [] = []
compactar (cabeca:resto) = compactar_aux cabeca 1 resto
  where
    compactar_aux atual contagem [] = [formatar atual contagem]
    compactar_aux atual contagem (proximo:resto_lista)
      | atual == proximo = compactar_aux atual (contagem + 1) resto_lista 
      | otherwise        = (formatar atual contagem) : compactar_aux proximo 1 resto_lista 
    formatar el 1 = [el] -- Contagem 1
    formatar el cnt = [ fromIntegral cnt, el] -- Contagem > 1
    
