module Printer where

import Prelude hiding (exp)

import AST
import Parser

import Data.IORef
import Control.Applicative( (<$>), (<*>) )


pprint :: Exp -> IO String

str <++> m = (str ++) <$> m

pprint (EIORef r) = do
  e <- readIORef r
  pprint e

pprint (Var ident) = return $ " `" ++ ident ++ "'"
pprint (Lambda ident exp) = ("λ" ++ ident ++ " → ") <++> (pprint exp)

pprint (Literal (LitInt a)) = return $ ' ' : show a
pprint (Literal (LitSybmol a)) = return a
pprint (Literal (LitChar a)) = return [a]

pprint (Cons x xs) = do
  px <- pprint x
  pt <- pprintListContent xs
  return $ "[" ++ px ++ pt ++ "]"

--pprint x = return $ show x
pprint x = return $ "(...)"

pprintListContent (Cons x xs) = (++) <$> (pprint x) <*> (pprintListContent xs)
pprintListContent (Literal (LitSybmol "Nil")) = return ""
pprintListContent _ = return " <not evaled expr>"

--pprint App e1 e2 = (pprint e1) ++ " (" ++ (pprint e2) ++ ") "
--pprint Case exp opts = "case " ++ (pprint exp) ++ " of "


