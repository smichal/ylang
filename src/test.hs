
import Test.HUnit
import Parser
import Text.ParserCombinators.Parsec

-- tsExpr :: String -> Either ParseError Exp
ts parser = parse parser ""

instance Eq ParseError where
  a == b = True

tests =
  test [
    "var" ~: ts expr "a " ~?= Right (Var "a"),
    "lambda" ~: ts expr "\\x -> a " ~?= Right (Lambda "x" (Var "a")),
    "application" ~: ts expr "a b c" ~?= Right (App (App (Var "a") (Var "b")) (Var "c")),

    "symbol literal" ~: ts expr "Asd " ~?= Right (Literal (LitSybmol "Asd")),
    "number literal" ~: ts expr "-12 " ~?= Right (Literal (LitInt (-12))),

    "pattern var" ~: ts pattern "a" ~?= Right (PatternVar "a"),
    "pattern const" ~: ts pattern "1" ~?= Right (PatternConst $ LitInt 1),
    "pattern list" ~: ts pattern "[a 0]" ~?= Right (PatternList [PatternVar "a",PatternConst (LitInt 0)])
   ]

main = runTestTT tests


