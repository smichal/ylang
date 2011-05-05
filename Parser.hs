module Parser where

import Text.ParserCombinators.Parsec
import qualified Text.ParserCombinators.Parsec.Token as P
import Text.ParserCombinators.Parsec.Language ( emptyDef )
import Text.ParserCombinators.Parsec.Expr

import Control.Applicative( (<$>), (<*>) )


--
-- AST
--
newtype Program = Program [Decl] deriving Show

type Ident = String

data Decl = Decl Ident Pattern Exp deriving (Show)

data Pattern
    = PatternList [Pattern] -- PatternList Pattern Pattern
    | PatternVar Ident
    | PatternConst Lit
    | PatternListTail Ident
    deriving (Show)

data Exp
    = Var Ident
    | Lambda Ident Exp
    | App Exp Exp
    | Case Exp [(Pattern, Exp)]
    | Literal Lit
    | LetIn [Decl] Exp
    | Cons Exp Exp
    | InternalFn (Exp -> Either String Exp)
    deriving (Show)

data Lit
    = LitInt Integer
    | LitSybmol String
    deriving (Show, Eq)


instance Show (a -> b) where
    show _ = "<fun>"


--
-- Lexer
--

lexer :: P.TokenParser ()
lexer = P.makeTokenParser (emptyDef {
            P.identStart = lower <|> (char '_') <|> (char '\''),
            P.identLetter = alphaNum <|> oneOf ":!#$%&*+./<=>?@\\^|-~",
            P.reservedOpNames = ["*","/","+","-", ":", "=="],
            P.reservedNames = ["let", "in", "case", "of", "->"] })
-- "\\" "->"


whiteSpace= P.whiteSpace lexer
lexeme    = P.lexeme lexer
integer   = P.integer lexer
identifier= P.identifier lexer
reserved  = P.reserved lexer
reservedOp= P.reservedOp lexer
semi      = P.semi lexer -- ;
parens    = P.parens lexer   -- ( )
brackets  = P.brackets lexer -- [ ]
braces    = P.braces lexer -- { }

semiSep    = P.semiSep lexer

symbol    = P.symbol lexer
natural   = P.natural lexer


--
-- Parser
--

--
-- Expressions
var :: Parser Exp
var = Var <$> identifier <?> "variable"

lambda :: Parser Exp
lambda = do
    symbol "\\"
    x <- identifier
    symbol "->"
    body <- expr
    return $ Lambda x body

letIn :: Parser Exp
letIn = do
    reserved "let"
    decls <- semiSep declaration
    reserved "in"
    e <- expr
    return $ LetIn decls e

emptyListExp = Literal $ LitSybmol "Nil"

list :: Parser Exp
list = do
    items <- brackets $ many exprWithoutApp
    return $ foldr Cons emptyListExp items

caseExpr :: Parser Exp
caseExpr = do
    reserved "case"
    e <- expr
    reserved "of"
    opts <- sepEndBy option semi
    return $ Case e opts
  where
    option :: Parser (Pattern, Exp)
    option = do
        p <- pattern
        reserved "->"
        e <- expr
        return (p, e)


expr :: Parser Exp
expr = buildExpressionParser table exprWithoutInfix <?> "expression"

table = [[preOp "-"]
        ,[binOp "*" AssocLeft, binOp "/" AssocLeft]
        ,[binOp "+" AssocLeft, binOp "-" AssocLeft]
        ,[consOp]
        ,[binOp "==" AssocLeft]
        ,[binOp "||" AssocLeft, binOp "&&" AssocLeft]
        ,[binOp "$" AssocLeft]
        ]
        where
          binOp s assoc = Infix (do{ reservedOp s; return (\x -> \y -> App (App (Var ('\'':s)) x) y)} <?> "operator") assoc
          preOp s = Prefix (do{ reservedOp s; return (\x-> App (Var s) x) })
          consOp = Infix (do{ reservedOp ":"; return (\x -> \y -> Cons x y)} <?> "cons (:)") AssocRight


exprWithoutInfix :: Parser Exp
exprWithoutInfix = do
    exprs <- many1 exprWithoutApp
    return $ case length exprs of
                1 -> head exprs
                otherwise -> foldl1 App exprs

exprWithoutApp :: Parser Exp
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
literal :: Parser Lit
literal = litInt <|> litSymbol

litInt :: Parser Lit
litInt = LitInt <$> natural <?> "nubmer"

litSymbol :: Parser Lit
litSymbol = LitSybmol <$> do
    c <- upper
    cs <- lexeme $ many letter
    return (c : cs) <?> "symbol"

--
-- Patterns
pattern :: Parser Pattern
pattern = patternList
          <|> (PatternVar <$> identifier)
          <|> (PatternConst <$> literal)
          <|> patternListTail --XXXXX
          <?> "pattern"

patternList :: Parser Pattern
patternList = PatternList <$> (brackets $ many pattern)

{-patternList = PatternList <$> brackets $ do
    --(brackets $ many pattern)
    patterns <- many pattern
    patternTail <- many patternListTail
    return $ patterns ++ patternTail
-}
patternListTail :: Parser Pattern
patternListTail = do
    symbol "&"
    PatternListTail <$> identifier

--
-- Declations
declaration :: Parser Decl
declaration = do
    id <- identifier
    pats <- many pattern
    symbol "="
    ex <- expr
    return $ Decl id (PatternList pats) ex

--
-- Program
program :: Parser Program
program = do
    many space
    decls <- sepEndBy declaration semi
    return $ Program decls


