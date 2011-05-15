module AST where

import Data.IORef


newtype Program = Program [Decl] deriving Show

type Ident = String

data Decl = Decl Ident Pattern Exp deriving (Show)

data Pattern
    = PatternList [Pattern]
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
    | EIORef (IORef Exp)
    deriving (Show)

data Lit
    = LitInt Integer
    | LitChar Char
    | LitSybmol String
    deriving (Show, Eq, Ord)


instance Show (a -> b) where
    show _ = "<fun>"

instance Show (IORef a) where
    show _ = "<ref>"




