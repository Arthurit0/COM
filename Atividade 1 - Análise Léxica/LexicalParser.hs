import Text.Parsec
import Text.Parsec.Expr
import Text.Parsec.Language
import Text.Parsec.Token qualified as T

lingDef =
  emptyDef
    { T.commentStart = "{-",
      T.commentEnd = "-}",
      T.commentLine = "--",
      --   Tarefa de operador lógico
      T.reservedOpNames = ["+", "-", "/", "*", "&&", "||", "!"],
      --   Aula 24/04:
      T.identStart = letter <|> char '_',
      T.identLetter = alphaNum <|> char '-',
      T.reservedNames = ["while", "return"]
    }

-- Também na aula 24/04:

symbol = T.symbol lexico

identifier = T.identifier lexico

float = T.float lexico

reserved = T.reserved lexico

bloco = do symbol "{"; cs <- listaCmd; symbol "}"; return cs

listaCmd = do c <- comando; cs <- listaCmd; return c : cs <|> return []

comando = do { reserved "return"; e <- expr; return (Ret e) } <|> do reserved "while"; l <- exprl; b <- bloco; return (While l b)

opLogico = (reservedOp "&&" >> return (&&)) <|> (reservedOp "||" >> return (||))
exprl = try (do a <- fator; op <- opLogico; b <- fator; return (op a b))



-- end

lexico = T.makeTokenParser lingDef

natural = T.natural lexico

parens = T.parens lexico

reservedOp = T.reservedOp lexico

tabela =
  [ [prefix "-" negate], [prefix "!" not],
    [binario "*" (*) AssocLeft, binario "/" div AssocLeft],
    [binario "+" (+) AssocLeft, binario "-" (-) AssocLeft],
    [binario "&&" (&&) AssocLeft, binario "||" (||) AssocLeft]
  ]

binario name fun assoc = Infix (do reservedOp name; return fun) assoc

prefix name fun = Prefix (do reservedOp name; return fun)

expr =
  buildExpressionParser tabela fator
    <?> "expression"

fator = parens expr <|> natural <|> exprNeg <|> exprlExpr <?> "simple expression"

exprlExpr = do l <- exprl; return (l)

partida :: Parsec String u Integer
partida = do e <- comando; eof; return e

parserE e = runParser partida [] "Expressoes" e

parserExpr s = case parserE s of
  Left er -> print er
  Right v -> (print v)

main = do
  putStr "Expressão:"
  e <- getLine
  parserExpr e