import ParserForSemantic


-- Aqui é definido o tipo de dados 'Semantica', que é um monad que armazena uma string e um valor genérico 'a', mantem um registro de mensagens de erro ou aviso enquanto realiza cálculos com 'a'
data Semantica a = MS (String, a) deriving Show

-- A instância Functor para 'Semantica' permite que você aplique uma função a 'a' mantendo a mesma string 
instance Functor Semantica where
    fmap f (MS (s, a)) = MS(s, f a)

-- A instância Applicative permite combinar 'Semantica' mantendo a concatenação das strings e aplicando a função armazenada no primeiro 'Semantica' ao 'a' no segundo
instance Applicative Semantica where
    pure x = MS("", x)
    MS(s1, f) <*> MS(s2, x) = MS(s1 <> s2, f x)

-- A instância Monad permite combinar 'Semantica' em sequência, mantendo a concatenação das strings
instance Monad Semantica where
    MS(s, a) >>= f = let MS(s', b) = f a in MS (s++s', b)

-- Definindo cores como strings para formatar a saída de texto no console, e definindo strings de erro e aviso
red = "\x1b[31m"
yellow = "\x1b[33m"
reset = "\x1b[0m"

erro s = MS (red ++ "Erro: " ++ reset ++ s ++ "\n", ())
adv s = MS (yellow ++ "Aviso: " ++ reset ++ s ++ "\n", ())

-- Definição do tipo de dados 'VerTipo' que pode ser um de cinco tipos: 'I' para int, 'D' para double, 'S' para string, 'V' para void ou 'E' para expressão
data VerTipo = I | D | S | V | E deriving (Eq, Show)

printTipo I = "Int"
printTipo D = "Double"
printTipo S = "String"
printTipo V = "Void"

-- Função que retorna o tipo de uma variável
tipo t | t == TInt = I
       | t == TDouble = D
       | t == TString = S
       | t == TVoid = V
       | otherwise = E

-- Funções que extraem o ID e o Tipo de uma variável
getVarId (id :#: _) = id
getVarTipo (_ :#: t) = t

-- Funções que extraem o ID, o Tipo e os Parâmetros de uma função
funcId (id :->: _) = id
funcTipo (_ :->: (_, t)) = t
funcParametros (_ :->: (lp, _)) = lp

-- Funções que extraem o ID, a Variável e o Bloco de uma lista de funções
listaFuncId (id, _, _) = id
listaFuncVar (_, var, _) = var
listaFuncBloco (_, _, bloco) = bloco 

-- Função que processa o ID de uma variável numa lista de variáveis
procVarId _ [] = E
procVarId id (lv: lvs) = if getVarId lv == id
                then tipo (getVarTipo lv)
                else procVarId id lvs

-- Função que processa uma função numa lista de funções
procFuncInList id (fs:fss) = if funcId fs == id 
                       then fs 
                       else procFuncInList id fss

-- Função que processa o ID de uma função numa lista de funções
procFuncId _ [] = E
procFuncId id (fs:fss) = if funcId fs == id 
                         then tipo (funcTipo fs)
                         else procFuncId id fss

-- Função que retorna o tipo de uma constante
constante (Const (CInt a)) = I
constante (Neg (Const (CInt a))) = I
constante (Const (CDouble a)) = D
constante (Neg (Const (CDouble a))) = D
constante (Const (CString a)) = S
constante _ = E
 
-- Funções que extraem os operandos de uma expressão
exprA (a :+: _) = a
exprA (a :-: _) = a
exprA (a :*: _) = a
exprA (a :/: _) = a
exprA a = a

exprB (_ :+: b) = b
exprB (_ :-: b) = b
exprB (_ :*: b) = b
exprB (_ :/: b) = b
exprB b = b

-- Funções que retornam uma expressão com operadores e operandos
exprOper (_ :+: _) a b = (a :+: b)
exprOper (_ :-: _) a b = (a :-: b)
exprOper (_ :*: _) a b = (a :*: b)
exprOper (_ :/: _) a b = (a :/: b)

-- Funções que retornam uma string representando o operador de uma expressão
exprOperMsg (_ :+: _)  = "+"
exprOperMsg (_ :-: _)  = "-"
exprOperMsg (_ :*: _)  = "*"
exprOperMsg (_ :/: _)  = "/"

-- Funções que convertem um inteiro para double e vice-versa
intParaDouble (Neg c) = Neg (IntDouble c)
intParaDouble c = IntDouble c

doubleParaInt (Neg c) = Neg (DoubleInt c)
doubleParaInt c = DoubleInt c

-- Função "verExprBin" lida com a conversão de tipos e verifica se a operação é possível para os tipos fornecidos.
verExprBin id expr (ta, a) (tb, b) |ta == E && (tb == I || tb == D) = do return (tb, (exprOper expr a b))
                                    |(ta == I || ta == D) && tb == E = do return (ta, (exprOper expr a b))  
                                    |ta == D && tb == I = do adv ("Na função '" ++ id ++ "':\n" 
                                                                   ++ "houve conversão de 'Int' para 'Double' na operação '"
                                                                   ++ exprOperMsg expr ++ "'")
                                                             return (D, (exprOper expr a (intParaDouble b)))
                                    |ta == I && tb == D = do adv ("Na função '" ++ id ++ "':\n"
                                                                   ++ "houve conversão de 'Int' para 'Double' na operação '"
                                                                   ++ exprOperMsg expr ++ "'")
                                                             return (D, (exprOper expr (intParaDouble a) b))
                                    |ta == D && tb == D = do return (D, (exprOper expr a b))
                                    |ta == I && tb == I = do return  (I, (exprOper expr a b))
                                    |ta == S && (tb == I || tb == D) = do erro ("Na função '" ++ id ++ "':\n" 
                                                                                ++ "A operação " ++ exprOperMsg expr 
                                                                                ++ " não aceita 'String' nos seus operadores") 
                                                                          return (tb, (exprOper expr a b))
                                    |(ta == I || ta == D) && tb == S = do erro ("Na função '" ++ id ++ "':\n"
                                                                                ++ " A operação " ++ exprOperMsg expr 
                                                                                ++ " não aceita 'String' nos seus operadores") 
                                                                          return (ta, (exprOper expr a b))
                                    |ta == V || tb == V = do erro ("Na função '" ++ id ++ "':\n" 
                                                                    ++ "A operação " ++ exprOperMsg expr 
                                                                    ++ " não aceita procedimentos nos seus operadores")
                                                             return (E, (exprOper expr a b))
                                    |otherwise = return(E, (exprOper expr a b))

-- A função 'verParametros'' está verificando os parâmetros da chamada da função e gerando erros de acordo.
-- Se houver um número de parâmetros na chamada diferente do que na definição da função, um erro será gerado.
verParametros'' id' (Chamada id lp) lv fs = do if length lp == length numP
                                                  then do vlp <- verParametros' id id lp flp lv fs
                                                          return (Chamada id vlp)
                                               else if length lp > length numP
                                                   then do erro ("Na função '" ++ id ++ "':\n"
                                                                 ++ "Excesso de argumentos na chamada da função " ++ id ++ " ")
                                                           return (Chamada id lp)
                                               else do erro ("Na função '" ++ id ++ "':\n"
                                                             ++ "Falta de argumentos na chamada da função '" ++ id ++ "'")
                                                       return (Chamada id lp)
                                           where 
                                                 f = procFuncInList id fs
                                                 flp = funcParametros f
                                                 numP = funcParametros (procFuncInList id fs)

-- A função 'idChamada' extrai o id de uma chamada de função.
getIdChamada (Chamada id _) = id

-- A função 'verParametros'' está verificando cada parâmetro da chamada da função e convertendo o tipo, se necessário.
verParametros' _ _ [] [] _ _ = return []
verParametros' id id' (lp:lps) (fp:fps) lv fs = do vlp <- verExpr id' lp lv fs
                                                   vlps <- verParametros' id id' lps fps lv fs
                                                   if fst vlp == I && vp == D
                                                      then do adv ("Na função '" ++ id' ++ "':\n"
                                                                   ++ "Houve conversão de 'Int' para 'Double' na passagem do parâmetro '"
                                                                   ++ idP ++ "' da função '" ++ id ++ "'")
                                                              return ((intParaDouble (snd vlp)):vlps)
                                                   else if fst vlp == D && vp == I
                                                        then do adv ("Na função '" ++ id' ++ "':\n"
                                                                     ++ "Houve conversão de 'Double' para 'Int' na passagem do parâmetro '"
                                                                     ++ idP ++ "' da função '" ++ id ++ "'")
                                                                return ((doubleParaInt (snd vlp)):vlps)
                                                   else if fst vlp == S && vp /= S
                                                        then do erro ("Na função '" ++ id' ++ "':\n"
                                                                      ++ "O parâmetro '" ++ idP ++ "', na função '"
                                                                      ++id ++ "', do tipo '"
                                                                      ++ t ++ "' não pode ser atribuido com o tipo '" ++ printTipo (fst vlp) ++ "'" )
                                                                return ((snd vlp):vlps)
                                                   else return ((snd vlp):vlps)
                                           where 
                                                vp = tipo (getVarTipo fp)
                                                t = printTipo vp
                                                idP = getVarId fp 

-- A função 'verChamada' verifica se uma chamada de função está correta verificando se a função está definida.
verChamada id' (Chamada id lp) lv fs = do if f == E
                                          then do erro ("Na função '" ++ id' ++ "':\n"
                                                         ++ "A função '" ++ id ++ "' não está definida")
                                                  return chm 
                                          else do v <- verParametros'' id' chm lv fs
                                                  return v 
                                          where 
                                                chm = (Chamada id lp)
                                                f = procFuncId id fs

-- A função 'verExpr' verifica diferentes tipos de expressões.
verExpr _ (Const c) _ _ = return (constante (Const c), (Const c))

-- Verificando a expressão de id da variável
verExpr id' (IdVar id) lv _ = do if getVarTipo == E 
                                 then do erro("Na função '" ++ id' ++ "':\n"
                                               ++ "Referência indefinida para a variável '" ++ id ++ "'")
                                         return (E, (IdVar id))
                                 else return (getVarTipo, (IdVar id))
                          where 
                               getVarTipo = procVarId id lv

-- Verificando a chamada de função                               
verExpr id' (Chamada id lp) lv fs = do if funcTipo == E
                                       then do erro ("Na função '" ++ id' ++ "':\n"
                                                     ++ "Referência indefinida para a função '" ++ id ++ "'")
                                               return (E, (Chamada id lp))
                                       else do  vlp <- verChamada id' chm lv fs
                                                return (funcTipo, vlp)
                               where 
                                     chm = (Chamada id lp)
                                     funcTipo = procFuncId id fs
                                     fp = funcParametros (procFuncInList id fs)

-- Verificando outras expressões
verExpr id' expr lv fs = do a <- verExpr id' (exprA expr) lv fs
                            b <- verExpr id' (exprB expr) lv fs
                            verExprBin id' expr a b
                    
                          
-- Funções que extraem o primeiro e o segundo operando de uma operação relacional.
exprRelA (a :==: _) = a
exprRelA (a :/=: _) = a
exprRelA (a :<: _) = a
exprRelA (a :>: _) = a
exprRelA (a :<=: _) = a
exprRelA (a :>=: _) = a

exprRelB (_ :==:b) = b
exprRelB (_ :/=:b) = b
exprRelB (_ :<: b) = b
exprRelB (_ :>: b) = b
exprRelB (_ :<=:b) = b
exprRelB (_ :>=:b) = b

-- 'exprOperRel' constrói uma operação relacional com dois operandos dados.
exprOperRel (_ :==: _) a b = (a :==: b)
exprOperRel (_ :/=: _) a b = (a :/=: b)
exprOperRel (_ :<: _) a b = (a :<: b)
exprOperRel (_ :>: _) a b = (a :>: b)
exprOperRel (_ :<=: _) a b = (a :<=: b)
exprOperRel (_ :>=: _) a b = (a :>=: b)

-- 'exprOperRelMsg' fornece a representação textual do operador relacional.
exprOperRelMsg (_ :==: _) = "=="
exprOperRelMsg (_ :/=: _) = "/="
exprOperRelMsg (_ :<: _) = "<"
exprOperRelMsg (_ :>: _) = ">"
exprOperRelMsg (_ :<=: _) = "<="
exprOperRelMsg (_ :>=: _) = ">="

verExprR id' exprR lv fs = do a <- verExpr id' (exprRelA exprR) lv fs
                              b <-  verExpr id' (exprRelB exprR) lv fs
                              if fst a == D && fst b == I
                                then do adv ("Na função '" ++ id' ++ "':\n"
                                            ++ "Houve conversão de 'Int' para 'Double' na operação '"
                                            ++ exprOperRelMsg exprR ++ "'")
                                        return (exprOperRel exprR (snd a) (intParaDouble (snd b)))
                              else if fst a == I && fst b == D 
                                  then do 
                                         adv ("Na função '" ++ id' ++ "':\n"
                                            ++ " Houve conversão de 'Int' para 'Double' na operação '"
                                            ++ exprOperRelMsg exprR ++ "'")
                                         return (exprOperRel exprR (intParaDouble (snd a)) (snd b)) 
                              else if fst a == S && (fst b == I || fst b == D)
                                  then do erro ("Na função '" ++ id' ++ "':\n"
                                                 ++ "Operador " ++ exprOperRelMsg exprR 
                                                 ++ " só aceita o tipo 'String' se o mesmo estiver nos dois operadores")
                                          return (exprOperRel exprR (snd a) (snd b))
                              else if (fst a == I || fst a == D) && fst b == S
                                  then do erro ("Na função '" ++ id' ++ "':\n" 
                                                 ++ "Operador " ++ exprOperRelMsg exprR 
                                                 ++ " só aceita o tipo 'String' se o mesmo estiver nos dois operadores")
                                          return (exprOperRel exprR (snd a) (snd b))
                              else if fst a == V || fst b == V 
                                  then do erro ("Na função '" ++ id' ++ "':\n"
                                                 ++ "Operador " ++ exprOperRelMsg exprR 
                                                 ++ " só aceita o tipo 'String' se o mesmo estiver nos dois operadores")
                                          return (exprOperRel exprR (snd a) (snd b))
                              else return (exprOperRel exprR (snd a) (snd b))
                            

-- Funções que extraem respectivamente o primeiro e o segundo operandos de uma operação lógica.
exprLogiA (a :&: _) = a
exprLogiA (a :|: _) = a

exprLogiB (_ :&: b) = b
exprLogiB (_ :|: b) = b

-- 'exprLogiOper' constrói uma operação lógica com dois operandos dados.
exprLogiOper (_ :&: _) a b = (a :&: b)
exprLogiOper (_ :|: _) a b = (a :|: b)

-- 'verExprL' é a função principal que verifica as expressões envolvidas na operação lógica, também manipula expressões lógicas 'Not'.
verExprL id' (Rel exprR) lv fs = do vr <- verExprR id' exprR lv fs
                                    return (Rel vr)

verExprL id' (Not (Rel exprR)) lv fs = do vr <- verExprR id' exprR lv fs
                                          return (Not (Rel vr))

verExprL id' (Not exprL) lv fs = do a <- verExprL id' (exprLogiA exprL) lv fs
                                    b <- verExprL id' (exprLogiB exprL) lv fs
                                    return (Not ((exprLogiOper exprL a b)))

verExprL id' exprL lv fs = do a <- verExprL id' (exprLogiA exprL) lv fs
                              b <- verExprL id' (exprLogiB exprL) lv fs
                              return (exprLogiOper exprL a b)

-- 'elemExiste' verifica se um elemento existe em uma lista, usa uma função 'f' para comparar o elemento 'id' com cada elemento da lista.
elemExiste  _ i [] = False
elemExiste f id (e:es) |f id == f e = True
                       |otherwise = elemExiste f id es

-- 'verAtrib'' verifica a atribuição de uma expressão a uma variável.
-- Não considera conversões de tipo e verifica apenas a expressão.
verAtrib' id' (Atrib id expr) lv fs = do v <- verExpr id' expr lv fs
                                         return (Atrib id (snd v))

-- 'verAtrib' verifica a atribuição de uma expressão a uma variável.
-- Considera conversões de tipo.
verAtrib id' (Atrib id expr) lv fs = do v <- verExpr id' expr lv fs
                                        if fst v == I && var == D 
                                          then do adv ("Na função '" ++ id' ++ "':\n"
                                                        ++ "Houve conversão de 'Int' para 'Double' na atribuição de '" ++ id ++ "'")
                                                  return (Atrib id (intParaDouble (snd v)))
                                        else if fst v == D && var == I
                                           then do adv ("Na função '" ++ id' ++ "':\n"
                                                        ++ "Houve conversão de 'Double' para 'Int' na atribuição de '" ++ id ++ "'")
                                                   return (Atrib id (doubleParaInt (snd v)))
                                        else if fst v == S && var /= S
                                            then do erro ("Na função '" ++ id' ++ "':\n" 
                                                           ++ "A variável '" ++ id ++ "' do tipo " 
                                                           ++ t ++ " não pode ser atribuída com o tipo '" ++ printTipo (fst v) ++ "'")
                                                    return (Atrib id (snd v))
                                        else if fst v /= S && var == S
                                            then do erro ("Na função '" ++ id' ++ "':\n" 
                                                           ++ "A variável '" ++ id ++ "' do tipo " 
                                                           ++ t ++ " não pode ser atribuída com o tipo '" ++ printTipo (fst v) ++ "'")
                                                    return (Atrib id (snd v))
                                        else return (Atrib id (snd v))
                                 where 
                                        var = procVarId id lv
                                        t = printTipo var

-- 'verRet' verifica a expressão de retorno de uma função.
-- Considera conversões de tipo.
verRet id' (Ret (Just expr)) t lv fs = do v <- verExpr id' expr lv fs
                                          if fst v == I && t == D
                                             then do adv ("Na função '" ++ id' ++ "':\n"
                                                           ++ "Houve conversão de 'Int' para 'Double' no retorno")
                                                     return (Ret (Just (intParaDouble (snd v))))
                                          else if fst v == D && t == I
                                               then do adv ("Na função '" ++ id' ++ "':\n"
                                                             ++ "Houve conversão de 'Double' para 'Int' no retorno")
                                                       return (Ret (Just (doubleParaInt (snd v))))
                                          else if fst v /= t && not(t == V && expr == (Const (CInt 0)))
                                               then do erro ("Na função '" ++ id' ++ "':\n" 
                                                              ++ "Tipo do retorno é incompatível, espera-se '"
                                                              ++ printTipo t
                                                              ++ "' ao invés de '" ++ printTipo (fst v) ++ "'")
                                                       return (Ret (Just expr))
                                          else return (Ret (Just expr))

-- 'verProc'' verifica a chamada a um procedimento.
-- Verifica se o número de argumentos fornecidos é correto, também verifica cada argumento.
verProc' id' (Proc id lExpr) lv fs = do if length lExpr == length numP
                                        then do vlExpr <- verProc id' lExpr lp lv fs
                                                return (Proc id vlExpr)
                                        else if length lExpr > length numP
                                            then do erro ("Na função '" ++ id' ++ "':\n"
                                                          ++ "Excesso de argumentos na função '" ++ id ++ "'")
                                                    return (Proc id lExpr)
                                        else do erro ("Na função '" ++ id' ++ "':\n"
                                                      ++ "Falta de argumentos na chamada de função '" ++ id ++ "'")
                                                return (Proc id lExpr) 
                                      where 
                                           f = procFuncInList id fs
                                           lp = funcParametros f
                                           numP = funcParametros (procFuncInList id fs)

-- 'verProc' verifica os argumentos de uma chamada de procedimento.
-- Esta função verifica cada argumento e considera conversões de tipo.
verProc _ [] [] lv fs = return []
verProc id' (e:es) (p:ps) lv fs = do ve <- verExpr id' e lv fs
                                     ves <- verProc id' es ps lv fs
                                     if fst ve == I && vp == D
                                        then do adv ("Na função '" ++ id' ++ "':\n"
                                                     ++ "Houve conversão de 'Int' para 'Double' em '"
                                                     ++ id ++ "'")
                                                return ((intParaDouble (snd ve)):ves)
                                     else if fst ve == D && vp == I
                                        then do adv ("Na função '" ++ id' ++ "':\n"
                                                     ++ "Houve conversão de 'Double' para 'Int' em '"
                                                     ++ id ++ "'")
                                                return ((doubleParaInt (snd ve)):ves)
                                     else if fst ve == S && vp /= S
                                          then do erro ("Na função '" ++ id' ++ "':\n"
                                                       ++ "O parâmetro '" ++ id ++ "' do tipo '"
                                                       ++ t ++ "' não pode ser atribuido com o tipo '" ++ printTipo (fst ve) ++ "'")
                                                  return ((snd ve):ves) 
                                     else return ((snd ve):ves)                                    
                                 where
                                      vp = tipo (getVarTipo p)
                                      t = printTipo vp
                                      id = getVarId p

-- 'verComando' verifica a validade de um comando individual. O tipo de comando determina o tipo de verificação realizado.
-- Comandos: If, While, Atrib, Leitura, Imp, Ret, Proc

verComando id' (If exprL b1 b2) t lv fs = do vL <- verExprL id' exprL lv fs
                                             vb1 <- verBloco id' b1 t lv fs
                                             vb2 <- verBloco id' b2 t lv fs
                                             return ( If vL vb1 vb2)

verComando id' (While exprL b) t lv fs = do vL <- verExprL id' exprL lv fs
                                            vb <- verBloco id' b t lv fs
                                            return (While vL vb)

verComando id' (Atrib id expr) _ lv fs = if elemExiste getVarId var lv
                                        then do v <- verAtrib id' atr lv fs
                                                return v
                                        else do erro ("Na função '" ++ id' ++ "':\n"
                                                      ++ "O identificador '" ++ id 
                                                      ++ "' está indefinido")
                                                v' <- verAtrib' id' atr lv fs
                                                return v'
                                        where 
                                             atr = (Atrib id expr)
                                             var = (id :#: TVoid)
                                      
verComando _ (Leitura id) _ _ _ = return (Leitura id)

verComando id' (Imp expr) _ lv fs = do v <- verExpr id' expr lv fs
                                       return (Imp (snd v))

verComando id' (Ret (Just expr)) t lv fs = do v <- verRet id' r t lv fs
                                              return v
                                           where 
                                              r = (Ret (Just expr))

verComando id' (Ret Nothing) t _ _ = do if t /= V
                                        then do erro ("Na função '" ++ id' ++ "':\n" 
                                                       ++ "Tipo do retorno icompativel, se espera "
                                                       ++ printTipo t)
                                                return (Ret Nothing)
                                        else return (Ret Nothing)

verComando id' (Proc id lExpr) _ lv fs = do if f == E
                                              then do erro ("Na função '" ++ id' ++ "':\n"
                                                            ++ "A função '" ++ id ++ "' não está definida")
                                                      return prc
                                            else do v <- verProc' id' prc lv fs
                                                    return v
                                        where 
                                             prc = (Proc id lExpr)
                                             f = procFuncId id fs

-- 'verBloco' verifica um bloco de comandos, verificando cada comando individualmente.                                       
verBloco _ [] _ _ _ = return []
verBloco id (b:bs) t lv fs = do vb <- (verComando id b t lv fs)
                                vbs <- (verBloco id bs t lv fs)
                                return (vb:vbs)

-- 'verReptFunc' verifica a repetição de funções
verReptFunc [] = (False, "")
verReptFunc (fs:fss) |elemExiste funcId fs fss = (True, funcId fs)
                     |otherwise = verReptFunc fss

-- 'verReptVar' verifica a repetição de variáveis
verReptVar [] = (False, "")
verReptVar (lv:lvs) |elemExiste getVarId lv lvs = (True, getVarId lv)
                    |otherwise = verReptVar lvs

-- 'verReptFuncParametro' verifica a repetição de parâmetros de funções
verReptFuncParametro [] = return True
verReptFuncParametro (fs:fss) = do let v = verReptVar (funcParametros fs)
                                   if fst v
                                   then do erro ("Na função '" ++ id ++ "':\n"
                                                 ++ "O parâmetro '" ++ snd v ++ "' foi declarado mais de uma vez")
                                           return False
                                   else return True
                                where
                                        id = funcId fs

-- 'verFuncoes' verifica a repetição em listas de funções
verFuncoes fs = do let f = verReptFunc fs
                   if fst f
                   then do erro("A função '" ++ snd f ++ "' foi declarada mais de uma vez")
                           return False
                   else return True

-- 'verVariaveis' verifica a repetição em listas de variáveis
verVariaveis id' lv = do let v = verReptVar lv
                         if fst v
                         then do erro ("Na função '" ++ id' ++ "':\n"
                                        ++ "A variável '" ++ snd v ++ "' foi declarada mais de uma vez")
                                 return False
                         else return True                 

-- 'verFuncao' verifica uma definição de função individual.
verFuncao f fs = do vlv <- verVariaveis id lv
                    if vlv
                    then do vb <- verBloco id b t lv fs
                            return (id, lv, vb)
                    else return f
                    where 
                         id = listaFuncId f
                         t = procFuncId id fs
                         lv = (listaFuncVar f) ++ (funcParametros (procFuncInList id fs))
                         b = listaFuncBloco f

-- 'verListaFuncoes' verifica uma lista de definições de funções.
verListaFuncoes [] _ = return []
verListaFuncoes (lf:lfs) fs = do vlf <- (verFuncao lf fs)
                                 vlfs <- (verListaFuncoes lfs fs)
                                 return (vlf:vlfs)

-- 'semantico' realiza a verificação semântica de um programa. 
-- Verifica as funções, parâmetros de funções e variáveis, e em seguida, verifica a lista de funções e o bloco principal.
-- Se houver erros, o programa original é retornado. Caso contrário, a versão verificada é retornada.                     
semantico (Prog fs lf lv b) = do vfs <- verFuncoes fs
                                 vlp <- verReptFuncParametro fs
                                 vlv <- verVariaveis "main" lv
                                 if vfs
                                 then do 
                                        vlf <- verListaFuncoes lf fs
                                        if vlv
                                        then do
                                                vb <- verBloco "main" b V lv fs
                                                return (Prog fs vlf lv vb)
                                        else return (Prog fs vlf lv b) 
                                 else return (Prog fs lf lv b)


-- 'printSemantica' aplica a função 'semantico' a um programa e imprime a saída.
printSemantica p = do let sem = semantico p
                      case sem of
                        MS p -> printSemantica' p

-- 'printSemantica'' imprime a saída da análise semântica.
printSemantica' p = do putStrLn (fst p)
                       print (snd p)

main = do input <- readFile "codigo.txt"
          let sin = parserE input
          case sin of 
                Left er -> print er
                Right v -> printSemantica v
          
        