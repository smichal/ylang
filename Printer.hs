module Printer where

import Prelude hiding (exp)

import Parser


pprint :: Exp -> String

pprint (Var ident) = " `" ++ ident ++ "'"
pprint (Lambda ident exp) = "λ" ++ ident ++ " → " ++ (pprint exp)

pprint (Literal (LitInt a)) = ' ' : show a
pprint (Literal (LitSybmol a)) = a
pprint (Literal (LitChar a)) = [a]

pprint (Cons x xs) = "[" ++ (pprint x) ++ (showTail xs) ++ "]"
  where
    showTail (Cons x xs) = (pprint x) ++ (showTail xs)
    showTail (Literal (LitSybmol "Nil")) = ""
    showTail _ = " <not evaled expr>"

pprint x = show x --"(...)"
--pprint App e1 e2 = (pprint e1) ++ " (" ++ (pprint e2) ++ ") "
--pprint Case exp opts = "case " ++ (pprint exp) ++ " of "


