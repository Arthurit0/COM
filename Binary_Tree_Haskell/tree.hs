data Arvore a = No a (Arvore a) (Arvore a) | Folha

ins :: Ord t => t -> Arvore t -> Arvore t
ins e Folha = No e Folha Folha
ins e (No x esq dir)
  | e == x = No x esq dir
  | e < x = No x (ins e esq) dir
  | otherwise = No x esq (ins e dir)

inOrder :: Arvore a -> [a]
inOrder Folha = []
inOrder (No x esq dir) = inOrder esq ++ [x] ++ inOrder dir

preOrder :: Arvore a -> [a]
preOrder Folha = []
preOrder (No x esq dir) = [x] ++ preOrder esq ++ preOrder dir

postOrder :: Arvore a -> [a]
postOrder Folha = []
postOrder (No x esq dir) = postOrder dir ++ postOrder esq ++ [x]