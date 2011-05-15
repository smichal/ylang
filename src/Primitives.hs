module Primitives where

import AST
import Evaluator

import Control.Monad.Error
import qualified Data.Map as Map

--
-- Primitives

primitives :: [(Ident, Exp -> Either String Exp)]
primitives =
  [ ("`+", binaryIntOp (+))
  , ("`-", binaryIntOp (-))
  , ("`*", binaryIntOp (*))
  , ("`/", binaryIntOp (div))
  , ("``-", unaryIntOp (0-))
  , ("`==", binaryBoolOp (==))
  , ("`<", binaryBoolOp (<))
  , ("`$!", strictApply)
  , ("`:!", strictCons)
  ]
    where
      binaryIntOp op (Literal (LitInt x)) = return $ InternalFn $ unaryIntOp (op x)
      binaryIntOp _ _ = integerError
      unaryIntOp op (Literal (LitInt x)) = return $ Literal $ LitInt (op x)
      unaryIntOp _ _ = integerError
      integerError = throwError "Integer function called on not a number"

      binaryBoolOp op (Literal lit) = return $ InternalFn $ unaryBoolOp (op lit)
      binaryBoolOp _ _ = boolError
      unaryBoolOp op (Literal lit) = return $ Literal $ LitSybmol (if (op lit) then "True" else "False")
      unaryBoolOp _ _ = boolError
      boolError = throwError "Invalid argument for boolean function"

      strictApply fn = return $ InternalFn (\arg -> return $ App fn arg)

      strictCons head = return $ InternalFn (\tail -> return $ Cons head tail)

internalFunctionsEnv :: Env
internalFunctionsEnv = Map.fromList $ map (\(id, fn) -> (id, InternalFn fn)) primitives

createEnviroment :: Program -> Env
createEnviroment (Program decls) = addManyToEnv decls internalFunctionsEnv
