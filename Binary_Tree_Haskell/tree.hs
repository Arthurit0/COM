data Arvore a = No a (Arvore a) (Arvore a) | Folha
ins e Folha = No e Folha Folha
ins e (No x esq dir) | e == x = No x esq dir
                     | e < x = No x (ins e esq) dir 
                     | otherwise = No x esq (ins e dir)

-- emOrdem :: Arvore a -> [a]
-- emOrdem Folha = 
-- emOrdem (No x esq dir) =