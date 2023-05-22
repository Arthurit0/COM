import Data.Functor.Identity qualified
import Data.Maybe (fromMaybe)
import Text.Parsec (ParseError, Parsec, ParsecT, char, choice, option, optionMaybe, parse, sepBy, sepBy1, sepEndBy, spaces, string, try, (<?>), (<|>))
import Text.Parsec.Expr (Assoc (AssocLeft), Operator (Infix, Prefix), buildExpressionParser)
import Text.Parsec.Language (emptyDef)
import Text.Parsec.Token qualified as T
import Text.ParserCombinators.Parsec (ParseError, Parser, char, choice, option, optionMaybe, parse, sepBy, sepBy1, sepEndBy, spaces, string, (<?>), (<|>))

type Id = String
type Bloco = [Comando]

data Tipo = TDouble | TInt | TString | TVoid deriving (Show)
data TCons = CDouble Double | CInt Integer deriving (Show)
data Expr = Expr :+: Expr | Expr :-: Expr | Expr :*: Expr | Expr :/: Expr | Neg Expr | Const TCons | IdVar Id | Chamada Id [Expr] | Lit String deriving (Show)
data ExprR = Expr :==: Expr | Expr :/=: Expr | Expr :<: Expr | Expr :>: Expr | Expr :<=: Expr | Expr :>=: Expr deriving (Show)
data ExprL = ExprL :&: ExprL | ExprL :|: ExprL | Not ExprL | Rel ExprR deriving (Show)
data Var = Id :#: Tipo deriving (Show)
data Funcao = Id :->: ([Var], Tipo) deriving (Show)
data Programa = Prog [Funcao] [(Id, [Var], Bloco)] [Var] Bloco deriving (Show)
data Comando
  = If ExprL Bloco Bloco
  | While ExprL Bloco
  | Atrib Id Expr
  | Leitura Id
  | Imp Expr
  | Ret (Maybe Expr)
  | Proc Id [Expr]
  | Print Expr
  | ReadAction String
  deriving (Show)

lingDef :: T.GenLanguageDef String u Data.Functor.Identity.Identity
lingDef =
  emptyDef
    { T.commentStart = "{-",
      T.commentEnd = "-}",
      T.commentLine = "--",
      T.reservedOpNames = ["{", "}", "+", "-", "/", "*", "==", "/=", "<", ">", "<=", ">=", "&", "|", "!"],
      T.reservedNames = ["while", "return", "int", "string", "double"]
    }

lexico :: T.GenTokenParser String u Data.Functor.Identity.Identity
lexico = T.makeTokenParser lingDef

symbol :: String -> ParsecT String u Data.Functor.Identity.Identity String
symbol = T.symbol lexico

reserved :: String -> ParsecT String u Data.Functor.Identity.Identity ()
reserved = T.reserved lexico

reservedOp :: String -> ParsecT String u Data.Functor.Identity.Identity ()
reservedOp = T.reservedOp lexico

parens :: ParsecT String u Data.Functor.Identity.Identity a -> ParsecT String u Data.Functor.Identity.Identity a
parens = T.parens lexico

natural :: ParsecT String u Data.Functor.Identity.Identity Integer
natural = T.natural lexico

identifier :: ParsecT String u Data.Functor.Identity.Identity String
identifier = T.identifier lexico

tabelaA :: [[Operator String u Data.Functor.Identity.Identity Expr]]
tabelaA =
  [ [prefix "-" Neg],
    [binario "*" (:*:) AssocLeft, binario "/" (:/:) AssocLeft],
    [binario "+" (:+:) AssocLeft, binario "-" (:-:) AssocLeft]
  ]

tabelaL :: [[Operator String u Data.Functor.Identity.Identity ExprL]]
tabelaL =
  [ [prefix "!" Not],
    [binario "&" (:&:) AssocLeft],
    [binario "|" (:|:) AssocLeft]
  ]

opR :: ParsecT String u Data.Functor.Identity.Identity (Expr -> Expr -> ExprR)
opR =
  do reservedOp "=="; return (:==:)
    <|> do reservedOp "/="; return (:/=:)
    <|> do reservedOp "<"; return (:<:)
    <|> do reservedOp ">"; return (:>:)
    <|> do reservedOp ">="; return (:>=:)
    <|> do reservedOp "<="; return (:<=:)

prefix :: String -> (a -> a) -> Operator String u Data.Functor.Identity.Identity a
prefix name fun = Prefix (do reservedOp name; return fun)

binario :: String -> (a -> a -> a) -> Assoc -> Operator String u Data.Functor.Identity.Identity a
binario name fun = Infix (do reservedOp name; return fun)

fator :: ParsecT String u Data.Functor.Identity.Identity Expr
fator =
  parens expr
    <|> do Const . CInt <$> natural
    <?> "simple expression"

expr :: ParsecT String u Data.Functor.Identity.Identity Expr
expr = buildExpressionParser tabelaA fator <?> "expression"

exprL :: ParsecT String u Data.Functor.Identity.Identity ExprL
exprL = buildExpressionParser tabelaL exprR <?> "logical expression"

exprR :: ParsecT String u Data.Functor.Identity.Identity ExprL
exprR = do
  e1 <- expr
  o <- opR
  Rel . o e1 <$> expr <?> "relacional expression"

programa :: Parser Programa
programa = do
  funcoes <- listaFuncoes
  Prog funcoes [] [] <$> blocoPrincipal

listaFuncoes :: Parser [Funcao]
listaFuncoes = sepEndBy funcao (symbol ";")

funcao :: Parser Funcao
funcao = do
  tipo <- tipoRetorno
  id <- identifier
  parametros <- parens declParametros
  let idsParametros = map (\(t, v) -> v :#: t) parametros
  blocoPrincipal <- bloco
  return $ id :->: (idsParametros, tipo)

tipoRetorno :: Parser Tipo
tipoRetorno = tipo <|> (reserved "void" >> return TVoid)

declParametros :: Parsec String u [(Tipo, String)]
declParametros =
  Text.Parsec.try
    ( do
        tipoParam <- tipo
        id <- identifier
        [outrosParams] <- parametros
        return $ (tipoParam, id) : outrosParams
    )
    <|> return []

parametros :: ParsecT String u Data.Functor.Identity.Identity [[(Tipo, String)]]
parametros = option [] $ symbol "," >> sepBy1 declParametros (symbol ",")

blocoPrincipal :: Parser Bloco
blocoPrincipal = do
  symbol "{"
  cmds <- blocoPrincipal'
  symbol "}"
  return cmds

blocoPrincipal' :: Parser Bloco
blocoPrincipal' = do
  decls <- declaracoes <|> return []
  cmds <- listaCmd
  return (concat decls ++ cmds)

declaracoes :: ParsecT String u Data.Functor.Identity.Identity [[Comando]]
declaracoes = sepEndBy declaracao (symbol ";")

declaracao :: ParsecT String u Data.Functor.Identity.Identity [Comando]
declaracao = do
  tipo <- tipo
  ids <- listaId
  symbol ";"
  return $ map (\id -> Atrib id (defaultValue tipo)) ids

defaultValue :: Tipo -> Expr
defaultValue TInt = Const (CInt 0)
defaultValue TDouble = Const (CDouble 0.0)
defaultValue TString = Lit ""
defaultValue TVoid = error "Tipo 'void' não tem valor default!"

tipo :: Parsec String u Tipo
tipo =
  choice
    [ reserved "int" >> return TInt,
      reserved "string" >> return TString,
      reserved "double" >> return TDouble
    ]

listaId :: ParsecT String u Data.Functor.Identity.Identity [String]
listaId = do
  id <- identifier
  lista <- listaId' <|> return []
  return (id : lista)

listaId' :: ParsecT String u Data.Functor.Identity.Identity [String]
listaId' =
  do
    symbol ","
    id <- identifier
    lista <- listaId' <|> return []
    return (id : lista)
    <|> return []

bloco :: Parser Bloco
bloco = do
  symbol "{"
  cmds <- listaCmd
  symbol "}"
  return cmds

listaCmd :: Parser [Comando]
listaCmd = sepEndBy comando (symbol ";")

chamadaFuncao :: Parsec String u Comando
chamadaFuncao = do
  nome <- identifier
  args <- parens (listaParametros'' <|> return [])
  symbol ";"
  return $ Proc nome args

listaParametros :: ParsecT String u Data.Functor.Identity.Identity [Expr]
listaParametros = parens (expr `sepBy` symbol ",") <|> return []
listaParametros' :: Parsec String u [Expr]
listaParametros' = expr `sepBy` symbol ","
listaParametros'' :: Parsec String u [Expr]
listaParametros'' =
  (symbol "," >> listaParametros')
    <|> return []

comando :: Parser Comando
comando =
  choice
    [ Text.Parsec.try $ string "return" >> spaces >> (Ret <$> optionMaybe expr) <* char ';',
      Text.Parsec.try $ do
        string "if"
        spaces
        condicao <- exprL
        spaces
        bloco1 <- bloco
        spaces
        elseOp <- optionMaybe (string "else" >> spaces >> bloco)
        return $ If condicao bloco1 (fromMaybe [] elseOp),
      Text.Parsec.try $ do
        string "while"
        spaces
        condicao <- exprL
        spaces
        While condicao <$> bloco,
      Text.Parsec.try $ do
        ident <- identifier
        spaces
        char '='
        spaces
        exp <- expr
        char ';'
        return $ Atrib ident exp,
      Text.Parsec.try $ do
        string "print"
        spaces
        char '('
        exp <- expr
        char ')'
        char ';'
        return $ Print exp,
      Text.Parsec.try $ do
        string "read"
        spaces
        char '('
        ident <- identifier
        char ')'
        char ';'
        return $ Leitura ident,
      chamadaFuncao
    ]

tvzExpressao :: Parser (Maybe Expr)
tvzExpressao = optionMaybe expr

senao :: Parser Bloco
senao = (reserved "else" >> listaCmd) <|> return []

parsePrograma :: String -> Either ParseError Programa
parsePrograma = parse programa ""

testParser :: String -> IO ()
testParser input = case parsePrograma input of
  Left err -> putStrLn $ "Erro na análise: " ++ show err
  Right ast -> putStrLn $ "Árvore Sintática Abstrata:\n" ++ show ast

main :: IO ()
main = do
  putStr "Insira a expressão a ser analisada: "
  e <- getLine
  testParser e
