import Text.Parsec
import Text.Parsec.Expr
import Text.Parsec.Combinator
import qualified Text.Parsec.Token as T
import Text.Parsec.Language
import System.IO
import Control.Monad (when, unless)
import Data.Function (on)
import Data.List (groupBy, intercalate, sortBy, nub, (\\))


--Bruno Marchi Pires

--Monada para propagação de erros:
instance Functor M where fmap f(MS(s,a)) = MS(s, f a)

instance Applicative M where
 pure x = MS ("",x)
 MS(s1, f) <*> MS(s2,x) = MS(s1<>s2, f x)

data M a = MS (String, a) deriving Show

instance Monad M where
 return x = MS("",x)
 MS (s,a) >>= f = let MS(s',b) = f a in MS (s ++ s' ,b)

erro s = MS ("Erro >> " ++s, ())
adv s = MS ("Advertencia >> " ++s, ())
--

type Id = String

data Tipo = TDouble | TInt | TString | TVoid deriving Show

data TCons = CDouble Double | CInt Int deriving Show

data Expr = Expr :+: Expr | Expr :-: Expr | Expr :*: Expr | Expr :/: Expr | Neg Expr | Const TCons | IdVar Id | Chamada Id [Expr] |  Lit String | IntDouble Expr | DoubleInt Expr deriving Show

data ExprR = Expr :==: Expr | Expr :/=: Expr | Expr :<: Expr | Expr :>: Expr | Expr :<=: Expr | Expr :>=: Expr deriving Show

data ExprL = ExprL :&: ExprL | ExprL :|: ExprL | Not ExprL | Rel ExprR deriving Show

data Var = Id :#: Tipo deriving Show

data Funcao = Id :->: ([Var], Tipo) deriving Show

data Programa = Prog [Funcao] [(Id, [Var], Bloco)] [Var] Bloco deriving Show

type Bloco = [Comando]

data Comando = If ExprL Bloco Bloco | While ExprL Bloco | Atrib Id Expr | Leitura Id | Imp Expr | Ret (Maybe Expr) | Proc Id [Expr] deriving Show

lingDef = emptyDef
          { T.commentStart      = "{-"
            , T.commentEnd      = "-}"
            , T.commentLine     = "--"
            , T.identStart      = letter <|> char '_'
            , T.identLetter     = alphaNum <|> char '_'
            , T.reservedNames   = ["while", "return", "if", "else", "print", "read","int", "string", "double", "void"]
            , T.reservedOpNames = ["+", "-", "/", "*", "==", "/=", "<", ">", "<=", ">=", "&&", "||", "!", "="]
          }  

lexico = T.makeTokenParser lingDef

symbol        = T.symbol lexico
parens        = T.parens lexico
reserved      = T.reserved lexico
reservedOp    = T.reservedOp lexico
natural       = T.natural lexico
float         = T.float lexico
stringLiteral = T.stringLiteral lexico
integer       = T.integer lexico
charLiteral   = T.charLiteral lexico
semi          = T.semi lexico
comma         = T.comma lexico
identifier    = T.identifier lexico

--Operações Relacionais
op = do { reservedOp "==" >> return (:==:)}
    <|> do { reservedOp "/=" >> return (:/=:)}
    <|> do { reservedOp "<=" >> return (:<=:)}
    <|> do { reservedOp ">=" >> return (:>=:)}
    <|> do { reservedOp "<" >> return (:<:)}
    <|> do { reservedOp ">" >> return (:>:)}
    <?> "op é inválido !"

exprR = try(do {e1<-expr; o <- op; e2<-expr; return (o e1 e2)}) 

--Facilitadores
binario  name fun assoc = Infix (do{reservedOp name; return fun }) assoc
prefix   name fun       = Prefix (do{reservedOp name; return fun })

--Tabela Operações Lógicas
tabelaLogic= [[prefix "!" (Not)]
            , [binario "&&" (:&:) AssocLeft]
            , [binario "||" (:|:) AssocLeft]
           ]

exprL = buildExpressionParser tabelaLogic logicTerms
        <?> "logic expression"

logicTerms =  parens exprL
          <|> do { e <- exprR; return (Rel e)}

--Tabela Operações Aritméticas
tabelaArit= [[prefix "-" (Neg)]
            , [binario "*" (:*:) AssocLeft, binario "/" (:/:) AssocLeft ]
            , [binario "+" (:+:) AssocLeft, binario "-" (:-:)   AssocLeft ]
           ]

fator = parens expr
       <|> do { str <- stringLiteral; return (Lit str)}
       <|> do { idv <- identifier; return (IdVar idv)}
       <|> try (do { db <- float; return (Const (CDouble db))})
       <|> try (do { n <- natural; return (Const (CInt (fromIntegral n)))})
       <|> do { c <- chamada; return c }
       <?> "simple expression"

expr = buildExpressionParser tabelaArit fator
       <?> "expression"   

--Produção Programa
programa = do
  listaPrototipos <- many prototipo
  (declaracoes, lsCmd) <- blocoPrincipal
  listaFuncoes <- many funcao
  return (Prog listaPrototipos listaFuncoes declaracoes lsCmd)

--Produção Função
funcao = do
  tipoRetorno <- tipoRetorno
  nome <-identifier
  parametros <- parens declParametros
  (dcl, lscmd) <- blocoPrincipal
  return (nome, dcl, lscmd)
  
prototipo = do
  tipoRetorno <- tipoRetorno
  nome <- identifier
  parametros <- parens declParametros
  semi
  return (nome :->: (parametros, tipoRetorno))

--Produção Tipo
tipo = do {reserved "int"; return TInt}
        <|> do {reserved "string"; return TString}
        <|> do {reserved "double"; return TDouble}

tipoRetorno = do {reserved "int"; return TInt}
        <|> do {reserved "string"; return TString}
        <|> do {reserved "double"; return TDouble}
        <|> do {reserved "void"; return TVoid}

--Produção <Bloco>
bloco = do 
    symbol "{"
    cs <- listaCmd
    symbol "}"
    return cs

--Produção <Bloco Principal>
blocoPrincipal = do 
    symbol "{"
    bp'<- blocoPrincipal'
    symbol "}"
    return bp'

blocoPrincipal' = do
    dcl <- declaracoes
    lsCmd <- listaCmd
    return (dcl,lsCmd)

-- Produção <Declaracoes>
declaracoes = do { tp <- tipo; lst <- listaId; semi; resto <- declaracoes; return (map (\id -> id :#: tp) lst ++ resto)}
                <|> return []

-- Produção <ListaId>
listaId = do
  id <- identifier
  listaIdResto <- listaId'
  return (id : listaIdResto)

-- Produção <ListaId'>
listaId' = do
  symbol ","
  listaId
  <|> return []

--Produção <DeclParametros>
declParametros = option [] $ do --option reconhece vazio
    tipo <- tipo
    nome <- identifier
    resto <- parametros
    return $ (nome :#: tipo) : resto

--parametros :: Parsec String u [(Tipo, Id)]
parametros = option [] $ do
    p <- declParametros
    ps <- many (comma >> declParametros)
    return (p ++ concat ps)

--Produção <ListaCmd>
listaCmd = do { c <- comando; cs<-listaCmd; return (c:cs)}
            <|> return []

--Produção <Senao> 
elseCmd = do { reserved "else"; pbl <-bloco; return pbl}
          <|> return []

--Comandos    
comando = do {reserved "return"; e<- optionMaybe expr; semi; return (Ret e)}
            <|> do { reserved "while"; l <- parens exprL; b <- bloco; return (While l b)}
            <|> try (do { reserved "if"; cond <- parens exprL; pbl<- bloco; sbl <- elseCmd; return (If cond pbl sbl)})
            <|> do { reserved "print"; string <- parens expr; semi; return (Imp string)}
            <|> do { reserved "read"; v <- parens identifier; semi; return (Leitura v) }
            <|> try (do { v <- identifier; reserved "="; e <- expr; semi; return (Atrib v e)})
            <|> try (do { v <- identifier; reserved "="; c <- chamada; semi; return (Atrib v c)})
            <|> try (do { (funcName, args) <- chamadaFuncao; semi; return (Proc funcName args)})

chamada = do
  id <- identifier
  args <- parens listaParametros
  return (Chamada id args)

chamadaFuncao = do
  id <- identifier
  args <- parens listaParametros
  return (id, args)

listaParametros = listaParametros' <|> return []

listaParametros' = do
  e <- expr
  es <- listaParametros''
  return (e : es)

listaParametros'' = do
  symbol ","
  listaParametros'
  <|> return []

-- Parte Dois - Analisador Semântico

analisadorSemantico (Prog funcoes procVars vars bloco) = do
  verificarFuncoesUnicas procVars
  mapM_ verificarVariaveisFuncao procVars
  verificarVariaveisMain vars
  let blocoConvertido = converterComandos bloco
  return (Prog funcoes procVars vars blocoConvertido)

--Não existe tratamento de erros
converterComandos :: [Comando] -> [Comando]
converterComandos = map converterComando
  where
    converterComando :: Comando ->  Comando
    converterComando (Atrib id expr) = Atrib id (converterIntDoubleExpr expr)
    converterComando (While expr bloco) = While (converterExprL expr) (converterComandos bloco)
    converterComando (If expr blocoP blocoS) = If (converterExprL expr) (converterComandos blocoP) (converterComandos blocoS)
    converterComando comando = comando

converterExprL :: ExprL -> ExprL
converterExprL (expr1 :&: expr2) = converterExprL expr1 :&: converterExprL expr2
converterExprL (expr1 :|: expr2) = converterExprL expr1 :|: converterExprL expr2
converterExprL (Not expr) = Not (converterExprL expr)
converterExprL (Rel exprR) = Rel (converterExprR exprR)

converterExprR :: ExprR -> ExprR
converterExprR (expr1 :==: expr2) = converterIntDoubleExpr expr1 :==: converterIntDoubleExpr expr2
converterExprR (expr1 :/=: expr2) = converterIntDoubleExpr expr1 :/=: converterIntDoubleExpr expr2
converterExprR (expr1 :<: expr2) = converterIntDoubleExpr expr1 :<: converterIntDoubleExpr expr2
converterExprR (expr1 :>: expr2) = converterIntDoubleExpr expr1 :>: converterIntDoubleExpr expr2
converterExprR (expr1 :<=: expr2) = converterIntDoubleExpr expr1 :<=: converterIntDoubleExpr expr2
converterExprR (expr1 :>=: expr2) = converterIntDoubleExpr expr1 :>=: converterIntDoubleExpr expr2

converterIntDoubleExpr :: Expr -> Expr
converterIntDoubleExpr (e1 :+: e2) =
  let e1' = converterIntDoubleExpr e1
      e2' = converterIntDoubleExpr e2
  in case (e1', e2') of
       (Const (CDouble _), Const (CInt _)) -> e1' :+: IntDouble e2'
       (Const (CInt _), Const (CDouble _)) -> IntDouble e1' :+: e2'
       _ -> e1' :+: e2'

converterIntDoubleExpr (e1 :-: e2) =
  let e1' = converterIntDoubleExpr e1
      e2' = converterIntDoubleExpr e2
  in case (e1', e2') of
       (Const (CDouble _), Const (CInt _)) -> e1' :-: IntDouble e2'
       (Const (CInt _), Const (CDouble _)) -> IntDouble e1' :-: e2'
       _ -> e1' :-: e2'

converterIntDoubleExpr (e1 :*: e2) =
  let e1' = converterIntDoubleExpr e1
      e2' = converterIntDoubleExpr e2
  in case (e1', e2') of
       (Const (CDouble _), Const (CInt _)) -> e1' :*: IntDouble e2'
       (Const (CInt _), Const (CDouble _)) -> IntDouble e1' :*: e2'
       _ -> e1' :*: e2'

converterIntDoubleExpr (e1 :/: e2) =
  let e1' = converterIntDoubleExpr e1
      e2' = converterIntDoubleExpr e2
  in case (e1', e2') of
       (Const (CDouble _), Const (CInt _)) -> e1' :/: IntDouble e2'
       (Const (CInt _), Const (CDouble _)) -> IntDouble e1' :/: e2'
       _ -> e1' :/: e2'
    
converterIntDoubleExpr (Neg e) = Neg (converterIntDoubleExpr e)
converterIntDoubleExpr (Const (CInt n)) = Const (CInt n)
converterIntDoubleExpr (Const c) = Const c
converterIntDoubleExpr (IdVar id) = IdVar id
converterIntDoubleExpr (Chamada id exprs) = Chamada id (map converterIntDoubleExpr exprs)
converterIntDoubleExpr (Lit s) = Lit s
converterIntDoubleExpr (IntDouble e) = IntDouble (converterIntDoubleExpr e)
converterIntDoubleExpr (DoubleInt e) = DoubleInt (converterIntDoubleExpr e)

-- FUNCOES ---------------
verificarFuncoesUnicas :: [(Id, [Var], Bloco)] -> M ()
verificarFuncoesUnicas procVars = do
  let idsRepetidos = obterIdsRepetidos procVars
  when (not (null idsRepetidos)) $
    erro ("Funções repetidas encontradas: " ++ intercalate ", " idsRepetidos ++ "\n")

obterIdsRepetidos :: [(Id, [Var], Bloco)] -> [Id]
obterIdsRepetidos procVars =
  let ids = map (\(id, _, _) -> id) procVars
      idsRepetidos = encontrarDuplicatas' ids
  in idsRepetidos

encontrarDuplicatas' :: Eq a => [a] -> [a]
encontrarDuplicatas' xs = nub (xs \\ nub xs)
-- END FUNCOES ------------

-- VARIÁVEIS --------------
verificarVariaveisFuncao :: (Id, [Var], Bloco) -> M ()
verificarVariaveisFuncao (_, vars, _) = verificarVariaveisUnicas vars

verificarVariaveisMain :: [Var] -> M ()
verificarVariaveisMain vars = do
  let varsDuplicadas = obterVariaveisDuplicadas vars
  when (not (null varsDuplicadas)) $
    erro ("Variáveis Duplicadas no Bloco Principal: " ++ intercalate ", " varsDuplicadas ++ "\n")

verificarVariaveisUnicas :: [Var] -> M ()
verificarVariaveisUnicas vars = do
  let varsDuplicadas = obterVariaveisDuplicadas vars
  when (not (null varsDuplicadas)) $
    erro ("Variáveis duplicadas encontradas em uma Função: " ++ intercalate ", " varsDuplicadas ++ "\n")

obterVariaveisDuplicadas :: [Var] -> [Id]
obterVariaveisDuplicadas vars = 
  let varsOrdenadas = ordenarVars vars
      varsDuplicadas = encontrarDuplicatas varsOrdenadas
  in map getVarId varsDuplicadas

ordenarVars :: [Var] -> [Var]
ordenarVars = sortBy compararVars

compararVars :: Var -> Var -> Ordering
compararVars (id1 :#: _) (id2 :#: _) = compare id1 id2

encontrarDuplicatas :: [Var] -> [Var]
encontrarDuplicatas [] = []
encontrarDuplicatas [_] = []
encontrarDuplicatas (var1@(id1 :#: _) : var2@(id2 :#: _) : rest)
  | id1 == id2 = var1 : encontrarDuplicatas (var2 : rest)
  | otherwise = encontrarDuplicatas (var2 : rest)

groupById :: [Var] -> [[Var]]
groupById = groupBy ((==) `on` getVarId)

getVarIds :: [Var] -> [Id]
getVarIds = map getVarId

getVarId :: Var -> Id
getVarId (id :#: _) = id 
-- END VARIÁVEIS

-- Main Section
partida :: Parsec String u Programa
partida = do {e <- programa; eof; return e}

parserE e = runParser partida [] "Expressoes" e

parserExpr s = case parserE s of
                     Left er -> do 
                      erro (show er)
                      return (Prog [] [] [] [])
                     Right v -> do 
                      analisadorSemantico v
                     
main = do putStr "\n Lendo Arquivo...\n"
          handle <- openFile "entrada.txt" ReadMode
          contents <- hGetContents handle
          let resultado = parserExpr contents
          case resultado of 
            MS (s, p) -> do
              putStrLn s
              print p
          hClose handle