import Text.Parsec
import Text.Parsec.Expr
import qualified Text.Parsec.Token as T
import Text.Parsec.Language

type Id = String
data Tipo = TDouble | TInt | TString | TFloat | TVoid deriving Show
data TCons = CDouble Double | CInt Integer deriving Show
data Expr = Expr :+: Expr | Expr :-: Expr | Expr :*: Expr | Expr :/: Expr | Neg Expr | Const TCons | IdVar Id | Chamada Id [Expr] | Lit String deriving Show
data ExprR = Expr :==: Expr | Expr :/=: Expr | Expr :<: Expr | Expr :>: Expr |Expr :<=: Expr | Expr :>=: Expr deriving Show
data ExprL = ExprL :&: ExprL | ExprL :|: ExprL | Not ExprL | Rel ExprR deriving Show
data Var = Id :#: Tipo deriving Show
data Funcao = Id :->: ([Var], Tipo) deriving Show
data Programa = Prog [Funcao] [(Id, [Var], Bloco)] [Var] Bloco deriving Show
type Bloco = [Comando]
data Comando = If ExprL Bloco Bloco
                | While ExprL Bloco
                | Atrib Id Expr
                | Leitura Id
                | Imp Expr
                | Ret (Maybe Expr)
                | Proc Id [Expr]
                | Print Expr
                | ReadAction String
                    deriving Show


definicao = emptyDef
  {
    T.commentStart    = "{-",
    T.commentEnd      = "-}",
    T.commentLine     = "--",
    T.reservedOpNames = ["+", "-", "*", "/", "==", "/=", "<", "<=", ">", ">=", "&&", "||", "!", "="],
    T.reservedNames   = ["int", "double", "string", "void", "if", "else", "while", "read", "print", "return"]
  }


lexico = T.makeTokenParser definicao
numero = T.naturalOrFloat lexico
simbolo = T.symbol lexico
parenteses = T.parens lexico
operador = T.reservedOp lexico
literal = T.stringLiteral lexico
identificador = T.identifier lexico
comando = T.comma lexico
reservada = T.reserved lexico
chaves = T.braces lexico
pontovirgula = T.semi lexico

prefix name fun = Prefix (do {operador name; return fun})
binario name fun = Infix  (do {operador name; return fun})

tabela =[[prefix "-" Neg],
    [binario "*" (:*:) AssocLeft,
     binario "/" (:/:) AssocLeft],
    [binario "+" (:+:) AssocLeft,
     binario "-" (:-:) AssocLeft]]

tabelaL = [[prefix "!" Not],
    [binario "&&" (:&:) AssocLeft,
     binario "||" (:|:) AssocLeft]]

opRel = do {(operador "==" >> return (:==:))
      <|> (operador ">=" >> return (:>=:))
      <|> (operador "<=" >> return (:<=:))
      <|> (operador ">" >> return (:>:))
      <|> (operador "<" >> return (:<:))
      <|> (operador "/=" >> return (:/=:))
      <?> "operador relacional"}

list elemento = sepBy elemento comando

fator = do parenteses expr
        <|> constante 
        <|> Lit <$> literal 
        <|> try (do
              i <- identificador;
              e <- parenteses (list expr)
              return (Chamada i e) 
            )
        <|> IdVar <$> identificador 
        <?> "expressao simples"

constante = do {c <- numero; case c of Left  n -> return (Const (CInt n)); Right n -> return (Const (CDouble n))}

expr = buildExpressionParser tabela fator 
       <?> "expressao"

exprRel = do {
    e <- expr;
    r <- opRel; 
    r e <$> expr;}

exprLog = do {parenteses logico 
        <|> Rel 
        <$> exprRel}
logico = buildExpressionParser tabelaL exprLog 
      <?> "expressao logica"

tipo = do {(reservada "int" >> return TInt) 
          <|> (reservada "double" >> return TDouble) 
          <|> (reservada "string" >> return TString)
          <|> (reservada "float" >> return TFloat) 
          <?> "type"}

parametro = do
        t <- tipo
        i <- identificador
        return (i :#: t)

parametros = list parametro

declaracao = do
        t <- tipo
        i <- list identificador
        pontovirgula
        return (map (:#: t) i)

tipoRetorno = do
    tipo 
    <|> (reservada "void" >> return TVoid) 
    <?> "type"

funcao = do
    tipo <- tipoRetorno
    id <- identificador
    p <- parenteses parametros
    bloco <- chaves bloco'
    return (id :->: (p, tipo), (id, concat (fst bloco), snd bloco))

funcoes = do {f <- many funcao;
          return (unzip f)}

blocoPrincipal = do 
                b <- chaves bloco'
                return (concat (fst b), snd b)

bloco' = do 
            d <- many declaracao
            c <- many comOptions
            return (d, c)

bloco = chaves (many comOptions)

comIf = try (do
            reservada "if"
            l <- parenteses logico
            b <- bloco
            reservada "else"
            If l b <$> bloco
            ) 
            <|> do
                reservada "if"
                l <- parenteses logico
                b <- bloco
                return (If l b [])

comWhile = do 
                reservada "while"
                l <- parenteses logico
                While l <$> bloco

comAtrib = do 
             i <- identificador
             operador "="
             e <- expr
             pontovirgula
             return (Atrib i e)

comRead = do
              reservada "read"
              i <- parenteses identificador
              pontovirgula
              return (Leitura i)

comPrint = do
               reservada "print"
               e <- parenteses expr
               pontovirgula
               return (Print e)

comReturn = try (do
                reservada "return"
                e <- expr
                pontovirgula
                return (Ret (Just e))
                ) 
                <|> do 
                    reservada "return"
                    pontovirgula
                    return (Ret Nothing)

comCall = do
              i <- identificador
              e <- parenteses (list expr)
              pontovirgula
              return (Proc i e)

callatrib = try comAtrib 
            <|> comCall

comOptions = do 
            comIf 
          <|> comWhile 
          <|> callatrib 
          <|> comRead 
          <|> comPrint 
          <|> comReturn 
          <?> "command"

partida = do 
          f <- funcoes
          m <- blocoPrincipal
          return (Prog (fst f) (snd f) (fst m) (snd m))

programa = do
      e <- partida
      eof
      return e

parser string = case runParser programa [] "Expressions" string of 
    Left error -> print error
    Right x -> print x

runTeste = do
    e <- readFile "teste1.j--"
    parser e

main = do
  putStr "Insira a expressão a ser analisada: "
  e <- getLine
  parser e
