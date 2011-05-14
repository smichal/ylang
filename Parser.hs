module Parser where

import AST

import Text.Parsec
import qualified Text.Parsec.Token as P
import Text.Parsec.Language ( emptyDef )
import Text.Parsec.Expr

import Control.Monad.Reader
import Control.Applicative( (<$>), (<*>) )



--
-- Lexer
--

lexer :: P.TokenParser ()
lexer = P.makeTokenParser (emptyDef {
    P.commentLine = "--",
    P.identStart = lower <|> (char '_') <|> (char '`'),
    P.identLetter = alphaNum <|> oneOf opChars,
    P.opStart = oneOf opChars,
    P.opLetter = oneOf opChars,
    P.reservedOpNames = ["*","/","+","-", ":", "==", "$"],
    P.reservedNames = ["let", "in", "case", "of", "->"] })
  where
    opChars = ":!#$%&*+./<=>?@\\^|-~"

whiteSpace = P.whiteSpace lexer
lexeme     = P.lexeme lexer
integer    = P.integer lexer
identifier = P.identifier lexer
operator   = P.operator lexer
reserved   = P.reserved lexer
reservedOp = P.reservedOp lexer
semi       = P.semi lexer -- ;
parens     = P.parens lexer   -- ( )
brackets   = P.brackets lexer -- [ ]

semiSep    = P.semiSep lexer

symbol    = P.symbol lexer
natural   = P.natural lexer


--
-- Parser
--

type YParser = Parsec String ()

runYParser parser = runParser parser ()

--
-- Expressions
var :: YParser Exp
var = Var <$> identifier <?> "variable"

lambda :: YParser Exp
lambda = do
    symbol "\\"
    x <- identifier
    symbol "->"
    body <- expr
    return $ Lambda x body

letIn :: YParser Exp
letIn = do
    reserved "let"
    decls <- sepEndBy declaration semi
    reserved "in"
    e <- expr
    return $ LetIn decls e

emptyListExp = Literal $ LitSybmol "Nil"

list :: YParser Exp
list = do
    items <- listExprs <|> listChars
    return $ foldr Cons emptyListExp items
  where
    listExprs = brackets $ many exprWithoutApp
    listChars = do { char '"'; s <- many (noneOf "\""); char '"'; return $ map (Literal . LitChar) s }

caseExpr :: YParser Exp
caseExpr = do
    reserved "case"
    e <- expr
    reserved "of"
    opts <- sepEndBy option semi
    return $ Case e opts
  where
    option :: YParser (Pattern, Exp)
    option = do
        p <- pattern
        reserved "->"
        e <- expr
        return (p, e)


expr :: YParser Exp
expr = buildExpressionParser table exprWithoutInfix <?> "expression"

table = [[preOp "-", otherInfixOperator, otherPrefixOperator]
        ,[binOp "*" AssocLeft, binOp "/" AssocLeft]
        ,[binOp "+" AssocLeft, binOp "-" AssocLeft]
        ,[consOp]
        ,[binOp "==" AssocLeft]
        ,[binOp "||" AssocLeft, binOp "&&" AssocLeft]
        ,[binOp "$" AssocRight]
        ]
  where
    binOp s assoc = Infix (do{ reservedOp s; return (\x y -> App (App (Var ('`':s)) x) y)} <?> "operator") assoc
    preOp s = Prefix (do{ reservedOp s; return (\x-> App (Var ("``" ++ s)) x) })
    consOp = Infix (do{ reservedOp ":"; return (\x y -> Cons x y)} <?> "cons (:)") AssocRight
    otherInfixOperator = Infix (do{ op <- operator; return (\x y -> App (App (Var ('`':op)) x) y)} <?> "infix operator") AssocLeft
    otherPrefixOperator = Prefix (do{ op <- operator; return (\x -> App (Var ("``" ++ op)) x)} <?> "prefix operator")


exprWithoutInfix :: YParser Exp
exprWithoutInfix = do
    exprs <- many1 exprWithoutApp
    return $ case length exprs of
                1 -> head exprs
                otherwise -> foldl1 App exprs

exprWithoutApp :: YParser Exp
exprWithoutApp = parens expr
       <|> var
       <|> lambda
       <|> (Literal <$> literal)
       <|> letIn
       <|> list
       <|> caseExpr
       <?> "expression"


--
-- Literals
literal :: YParser Lit
literal = litInt <|> litChar <|> litSymbol

litInt :: YParser Lit
litInt = LitInt <$> natural <?> "nubmer"

litChar :: YParser Lit
litChar = do
    char '\''
    c <- anyChar
    char '\''
    return $ LitChar c

litSymbol :: YParser Lit
litSymbol = LitSybmol <$> do
    c <- upper
    cs <- lexeme $ many letter
    return (c : cs) <?> "symbol"

--
-- Patterns
pattern :: YParser Pattern
pattern = patternList
          <|> (PatternVar <$> identifier)
          <|> (PatternConst <$> literal)
          <|> patternListTail --XXXXX
          <?> "pattern"

patternList :: YParser Pattern
patternList = PatternList <$> (brackets $ many pattern)

patternListTail :: YParser Pattern
patternListTail = do
    symbol "&"
    PatternListTail <$> identifier

--
-- Declations
declaration :: YParser Decl
declaration = do
    id <- identifier
    pats <- many pattern
    symbol "="
    ex <- expr
    return $ Decl id (PatternList pats) ex

--
-- Program
program :: YParser Program
program = do
    many space
    decls <- sepEndBy declaration semi
    eof
    return $ Program decls


