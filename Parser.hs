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

data Decl = Decl Ident [Pattern] Exp deriving (Show)

data Pattern
    = PatternList [Pattern] -- PatternList Pattern Pattern
    | PatternVar Ident
    | PatternConst Lit
    deriving (Show)

data Exp
    = Var Ident
    | Lambda Ident Exp
    | App Exp Exp
    | Case Exp [(Pattern, Exp)]
    | Literal Lit
    | LetIn [Decl] Exp
    | InternalFn (Exp -> Either String Exp)
    deriving (Show)
    -- Cons Exp Exp

data Lit
    = LitInt Integer
    | LitSybmol String
    deriving (Show)


instance Show (a -> b) where
    show _ = "<fun>"


--
-- Lexer
--

lexer :: P.TokenParser ()
lexer = P.makeTokenParser (emptyDef {
            P.identStart = lower,
            P.reservedOpNames = ["*","/","+","-"],
            P.reservedNames = ["let", "in", "case"] })
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


expr :: Parser Exp
expr = buildExpressionParser table exprWithoutInfix <?> "expression"

table = [[preOp "-"]
        ,[binOp "*" AssocLeft, binOp "/" AssocLeft]
        ,[binOp "+" AssocLeft, binOp "-" AssocLeft]
        ]
        where
          binOp s assoc = Infix (do{ reservedOp s; return (\x -> \y -> App (App (Var s) x) y)} <?> "operator") assoc
          preOp s = Prefix (do{ reservedOp s; return (\x-> App (Var s) x) })


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
          <?> "pattern"

patternList :: Parser Pattern
patternList = PatternList <$> (brackets $ many pattern)

--
-- Declations
declaration :: Parser Decl
declaration = do
    id <- identifier
    pat <- many pattern
    symbol "="
    ex <- expr
    return $ Decl id pat ex

--
-- Program
program :: Parser Program
program = Program <$> semiSep declaration


