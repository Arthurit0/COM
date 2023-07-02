module ParserForSemantic (
       Id,
       Tipo(..),
       TCons(..),
       Expr(..),
       ExprR(..),
       ExprL(..),
       Var(..),
       Funcao(..),
       Programa(..),
       Bloco(..),
       Comando(..),
       parserE,
       parserExpr,
       partida
) where


import Text.Parsec
import Text.Parsec.Expr
import qualified Text.Parsec.Token as T
import Text.Parsec.Language

-- Definição dos tipos e estruturas de dados:
type Id = String
data Tipo = TDouble|TFloat |TInt |TString |TVoid deriving (Eq, Show)
data TCons = CDouble Double |CInt Int |CString String deriving (Eq, Show)
data Expr = Expr :+: Expr |Expr :-: Expr |Expr :*: Expr |Expr :/: Expr |Neg Expr |Const TCons |IdVar Id |Chamada Id [Expr] |Lit String |IntDouble Expr |DoubleInt Expr deriving (Eq, Show)
data ExprR = Expr :==: Expr |Expr :/=: Expr | Expr :<: Expr | Expr :>: Expr | Expr :<=: Expr | Expr :>=: Expr deriving (Eq, Show)
data ExprL = ExprL :&: ExprL | ExprL :|: ExprL | Not ExprL | Rel ExprR deriving (Eq, Show)
data Var = Id :#: Tipo deriving (Eq, Show)
data Funcao = Id :->: ([Var], Tipo) deriving (Eq, Show)     
data Programa = Prog [Funcao] [(Id, [Var], Bloco)] [Var] Bloco deriving Show
type Bloco = [Comando]
data Comando = If ExprL Bloco Bloco | While ExprL Bloco | Atrib Id Expr | Leitura Id | Imp Expr | Ret (Maybe Expr) | Proc Id [Expr] deriving (Eq, Show)

-- Definições da linguagem: configuração de palavras reservadas
definicao = emptyDef{
         T.commentStart = "{-",
         T.commentEnd = "-}",
         T.commentLine = "--",
         T.identStart = letter<|> char '_', 
         T.identLetter = alphaNum <|> char '_',
         T.reservedNames = ["if", "else", "return", "while", "id",
                            "print", "read", "int", "string", "double", "float", "void"]
        }

-- Criando um lexer com base nas definições da linguagem
lexico = T.makeTokenParser definicao

-- Funções auxiliares para analisar tokens
natural = T.natural lexico
simbolo = T.symbol lexico
parenteses = T.parens lexico
operador = T.reservedOp lexico
identificador = T.identifier lexico
pontoVirgula = T.semi lexico
virgula = T.comma lexico
inteiro = T.integer lexico
flutuante = T.float lexico
literal = T.stringLiteral lexico
reservada = T.reserved lexico

-- Definição de operações e precedência em tabela
tabela   = [[prefix "-" Neg], 
            [binario "*" (:*:) AssocLeft, binario "/" (:/:) AssocLeft ],
            [binario "+" (:+:) AssocLeft, binario "-" (:-:) AssocLeft ]
           ]
tabelaL = [[prefix "!" Not],
           [binario "&&" (:&:) AssocLeft],
           [binario "||" (:|:) AssocLeft]
          ]

-- Funções auxiliares para lidar com operações prefixas e binárias
binario name fun assoc = Infix (do{operador name; return fun }) assoc
prefix name fun = Prefix (do{operador name; return fun })

-- A função 'expr' utiliza a tabela de operadores 'tabela' e o parser 'fator' para construir um parser de expressões aritméticas.
expr = buildExpressionParser tabela fator
       <?> "expression"   

-- A função 'fator' tenta analisar expressões entre parênteses, identificadores, números ou strings literais.
fator = parenteses expr
       <|> fatorTestaId
       <|> fatorTentaNum
       <|> do{s <- literal; return (Const (CString s))}
       <?> "simple expression"

-- 'fatorTestaId' tenta analisar uma chamada de função ou um identificador
fatorTestaId = try fatorChamadaFuncao <|> fatorVariavel
fatorVariavel = do{id <- identificador; return (IdVar id)}
fatorChamadaFuncao = do{id <- identificador; vs <- parenteses (listaParametros); return (Chamada id vs)}

-- 'fatorTentaNum' tenta analisar um número flutuante ou um número inteiro
fatorTentaNum = try fatorFloat <|> fatorInteiro
fatorInteiro = do {n <- natural; return (Const (CInt (fromIntegral n)))}
fatorFloat = do {f <- flutuante; return (Const (CDouble f))}

-- 'exprR' analisa expressões de relação, que comparam duas expressões com um operador
exprR = do{e1 <- expr; o <- op; e2 <- expr; return (Rel (o e1 e2))}

-- 'op' analisa operadores de comparação
op = do {operador "=="; return (:==:)}
     <|> do {operador "/="; return (:/=:)}
     <|> do {operador "<"; return (:<:)}
     <|> do {operador ">"; return (:>:)}
     <|> do {operador "<="; return (:<=:)}
     <|> do {operador ">="; return (:>=:)}

-- 'exprL' analisa expressões lógicas utilizando a tabela de operadores 'tabelaL' e o parser 'fatorL'
exprL = buildExpressionParser tabelaL fatorL 

fatorL = parenteses exprL
        <|> do{n <- exprR; return n}
        <?> "simple expression"

-- 'blocoPrincipal' e 'blocoPrincipal'' analisam o bloco principal de código que consiste em uma lista de declarações e uma lista de comandos
blocoPrincipal = do simbolo "{"
                    bs <- blocoPrincipal'
                    simbolo "}"
                    return bs

blocoPrincipal' = do{d <- declaracoes; l <- listaCmd; return (d, l)}

-- A função 'declaracoes' analisa declarações de variáveis
declaracoes = do{t <- tipo; id <- listaId; pontoVirgula; ds <- declaracoes; return ([(x:#:t) | x <- id] ++ ds)}
              <|> return []

-- 'listaId' e 'listaId'' analisam listas de identificadores, separados por vírgulas
listaId = do{id <- identificador; ids <- listaId'; return (id:ids)} 

listaId' = do{virgula; listaId}
           <|> return []

-- A função 'bloco' analisa um bloco de código, que é uma lista de comandos delimitada por chaves
bloco = do simbolo "{"
           cs <- listaCmd
           simbolo "}"
           return cs

-- 'listaCmd' analisa uma lista de comandos
listaCmd = do {c <- comando; cs <- listaCmd; return (c:cs)}
          <|> return []

-- 'comando' analisa vários tipos de comandos, incluindo retorno, condicionais, laços, print, leitura e atribuições
comando = do{reservada "return"; e <- expressaoVazia; pontoVirgula; return e}
          <|>do{reservada "if"; l <- parenteses exprL; b <- bloco; s <- senao; return (If l b s)}
          <|>do{reservada "while"; l <- parenteses exprL; b <- bloco; return (While l b)}
          <|>do{reservada "print"; e <- parenteses expr; pontoVirgula; return (Imp e)}
          <|>do{reservada "read"; id <- parenteses (identificador); pontoVirgula; return (Leitura id) }
          <|>comandoTry

-- 'comandoTry' tenta analisar uma atribuição a um identificador ou uma chamada de função
comandoTry = try comandoId <|> comandoFuncao

comandoId = do{id <- identificador; simbolo "="; e <- expr; pontoVirgula; return (Atrib id e)}
comandoFuncao = do{id <- identificador; l <- parenteses (listaParametros); pontoVirgula; return (Proc id l)}

-- 'expressaoVazia' analisa uma expressão opcionalmente vazia
expressaoVazia = do{e <- expr; return (Ret (Just e))}
               <|> return (Ret (Nothing))

-- 'listaParametros', 'listaParametros'' e 'listaParametros''' analisam uma lista de parâmetros de uma função
listaParametros = do{l <- listaParametros'; return l}
                  <|> return []

listaParametros' = do{e <- expr; l <- listaParametros''; return (e:l)}

listaParametros'' = do{virgula; l <-listaParametros'; return l}
                     <|> return []

-- 'senao' analisa um comando else opcionalmente vazio
senao = do{reservada "else"; b <- bloco; return b}
        <|>do {return []}
 
-- 'tipo' analisa os tipos de variáveis
tipo = do{reservada "int"; return TInt}
       <|>do{reservada "double"; return TDouble}
       <|>do{reservada "string"; return TString}
       <|>do{reservada "float"; return TFloat}

-- 'tipoRetorno' analisa o tipo de retorno de uma função
tipoRetorno = do{t <- tipo; return t}
              <|>do{reservada "void"; return TVoid}

-- 'funcao' analisa a declaração de uma função
funcao = do t <- tipoRetorno
            id <- identificador
            p <- parenteses declParametros
            b <- blocoPrincipal
            return ((id :->: (p, t)), id, b)

-- 'funcoes' analisa uma lista de funções
funcoes = do{f <- funcao; fs <- funcoes; return (f:fs)}
          <|> return []

-- 'f_id', 'f_fun', 'f_bp_d' e 'f_bp_l' são funções auxiliares para extrair componentes de uma função
f_id (_, i, _) = i
f_fun (f, _, _) = f
f_bp_d (_, _, (d, _)) = d
f_bp_l (_, _, (_, l)) = l

-- 'funcoesListaFuncao' e 'funcoesTripla' transformam uma lista de funções em outras representações
funcoesListaFuncao funcoes = return [(f_fun x) | x <- funcoes]

funcoesTripla funcoes = return [( f_id x, f_bp_d x, f_bp_l x) | x <- funcoes]

-- 'parametro' e 'declParametros' analisam declarações de parâmetros de funções
parametro = do{t <- tipo; id <- identificador; return (id:#:t)}

declParametros = do{p <- parametro; ps <- parametros; return (p:ps)}
                 <|> return []

parametros = do{virgula; declParametros}
             <|> return []

-- 'programa' analisa um programa completo, que consiste em uma lista de funções seguida de um bloco principal de código
programa = do f <- funcoes
              bp <- blocoPrincipal
              fl <- funcoesListaFuncao f
              fb <- funcoesTripla f
              return (Prog fl fb (fst bp) (snd bp))


-- Função para iniciar a análise sintática:
partida = do {e <- programa; eof; return e}

-- Funções para executar o parser e manipular os resultados
parserE e = runParser partida [] "Expressões" e
 
parserExpr s = case parserE s of
                     Left er -> print er
                     Right v -> (print v)

main = do
    arquivo <- readFile "codigo.txt"
    parserExpr arquivo
