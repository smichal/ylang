module Evaluator where

import Prelude hiding (exp)

import Parser

import Control.Monad.Reader
import Control.Monad.Error
import qualified Data.Map as Map


type Env = Map.Map Ident Exp
type Evaluator = ErrorT String (Reader Env) Exp

runEvaluator :: Evaluator -> Env -> Either String Exp
runEvaluator evaluator env = runReader (runErrorT evaluator) env

createEnviroment :: Program -> Env
createEnviroment (Program decls) = addManyToEnv decls internalFunctionsEnv

addManyToEnv :: [Decl] -> Env -> Env
addManyToEnv decls env = foldl addToEnv env decls

addToEnv :: Env -> Decl -> Env
addToEnv env (Decl id patterns exp) = Map.insert id (expandPatterns patterns exp) env


lookupIdent :: Ident -> Evaluator
lookupIdent ident = do
  maybeVal <- asks $ Map.lookup ident
  case maybeVal of
    Just exp -> return exp
    Nothing -> throwError $ "No value binded to symbol '" ++ ident ++ "'"

bindIdent :: Ident -> Exp -> Env -> Env
bindIdent = Map.insert

eval :: Exp -> Evaluator

--consOp = Var ":"

--eval e @ (Literal _) = return e
eval e @ (App (App (Var ":") _head_) _tail_) = return e

eval (App fnExp argExp) = do
  fn <- eval fnExp
  case fn of
    (Lambda argName body) -> return $ substitue argName argExp body
    (InternalFn fn) -> do
      evaled <- strictEval argExp
      case fn evaled of
        Right exp -> return exp
        Left err -> throwError err
    otherwise ->
      --return $ App fn argExp
      throwError ("Not a function " ++ (show fn))

eval (Var varName) = do
  value <- lookupIdent varName
  return value

eval (LetIn decls exp) = local (addManyToEnv decls) (eval exp)


eval e = return e

strictEval :: Exp -> Evaluator
strictEval exp = case exp of
  Lambda _ _   -> return exp
  Literal _    -> return exp
  InternalFn _ -> return exp
  otherwise    -> strictEval =<< eval exp


substitue :: Ident -> Exp -> Exp -> Exp
substitue ident target exp =
  case exp of
    Var id | id == ident -> target
    Lambda id body | id /= ident -> Lambda id (subs body)
    App exp1 exp2 -> App (subs exp1) (subs exp2)
    --Case exp1 options = Case (subs exp1) map (\(
    --LetIn
    other -> exp
  where subs = substitue ident target


-- f x 0 = ..
-- is expanding into: f = \x -> \y -> case y of 0 -> ...
expandPatterns :: [Pattern] -> Exp -> Exp
expandPatterns [] exp = exp
expandPatterns ((PatternVar ident) : rest) exp = Lambda ident $ expandPatterns rest exp
expandPatterns (pattern : rest) exp = Lambda "_x" $ Case (Var "_x") [(pattern, expandPatterns rest exp)]




--
-- Primitives

primitives :: [(Ident, Exp -> Either String Exp)]
primitives =
  [ ("+", binaryIntOp (+))
  , ("-", binaryIntOp (-))
  , ("*", binaryIntOp (*))
  , ("/", binaryIntOp (div))
  ]
    where
      binaryIntOp op (Literal (LitInt x)) = return $ InternalFn $ unaryIntOp (op x)
      binaryIntOp _ _ = integerError
      unaryIntOp op (Literal (LitInt x)) = return $ Literal $ LitInt (op x)
      unaryIntOp _ _ = integerError
      integerError = throwError "Integer function called on not a number"

internalFunctionsEnv :: Env
internalFunctionsEnv = Map.fromList $ map (\(id, fn) -> (id, InternalFn fn)) primitives


tst = App (Lambda "y" (App (Var "id") (Var "y"))) (Var "x")
tstEnv = Map.union internalFunctionsEnv $ Map.fromList [("id", Lambda "z" (Var "z")), ("x", Literal $ LitInt 42)]
