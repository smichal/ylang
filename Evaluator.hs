module Evaluator where

import Prelude hiding (exp)

import Parser

import Control.Monad.Reader
import Control.Monad.Error
import qualified Data.Map as Map
import Data.List ( find, groupBy )
import Data.Maybe ( isJust, fromJust )
import Control.Applicative( (<$>), (<*>) )


import Debug.Trace

type Env = Map.Map Ident Exp
type Evaluator = ErrorT String (Reader Env)

runEvaluator :: Evaluator Exp -> Env -> Either String Exp
runEvaluator evaluator env = runReader (runErrorT evaluator) env

createEnviroment :: Program -> Env
createEnviroment (Program decls) = addManyToEnv decls internalFunctionsEnv

addManyToEnv :: [Decl] -> Env -> Env
addManyToEnv decls env = Map.union (Map.fromList $ expandDeclarations decls) env


-- add declaration to envitoment
-- when is adding:
--   f 0 = 1
--   f 1 = 3
--   f n = ..
-- it joins it to:
--   f = \x -> case x of 0 -> 1; 1 -> 3; n -> ... ;
--
--   XXX: \x -> \y -> case [x y] of ...
{-
addToEnv :: Env -> Decl -> Env
addToEnv env (Decl id patterns exp) = case Map.lookup id env of
  Just sth -> case (getCaseList expandedExp, getCaseList sth) of
    (Just newList, Just oldList) ->
      Map.insert id (Lambda "_x" (Case (Var "_x") (oldList ++ newList))) env
    other -> Map.insert id expandedExp env
  Nothing -> Map.insert id expandedExp env
  where
    expandedExp = expandPatterns patterns exp
    getCaseList exp = case exp of
      (Lambda x (Case (Var x') caseList)) | x == x' -> Just caseList
      other -> Nothing
-}


expandDeclarations :: [Decl] -> [(Ident, Exp)]
expandDeclarations decls = let
    ident (Decl a _ _) = a
    groups = groupBy (\a b -> (ident a) == (ident b)) decls
    ids :: [Ident]
    ids = map (ident . head) groups
    bodies :: [[(Pattern, Exp)]]
    bodies = map (map (\(Decl _ p e) -> (p,e))) groups
    exps = map expandPatterns bodies
  in
    zip ids exps



-- f x 0 = ...
-- is expanding into: f = \x1 -> \x2 -> case [x1 x2] of [x 0] -> ...
expandPatterns :: [(Pattern, Exp)] -> Exp
expandPatterns options = foldr Lambda caseExp argsNames
  where
    ((PatternList p, _) : _) = options
    argsNames = map (\i -> "_x" ++ (show i)) [1..(length p)]
    caseExp = Case
      (foldr Cons (Literal $ LitSybmol "Nil") (map Var argsNames))
      options




lookupIdent :: Ident -> Evaluator Exp
lookupIdent ident = do
  maybeVal <- asks $ Map.lookup ident
  case maybeVal of
    Just exp -> return exp
    Nothing -> throwError $ "No value binded to symbol '" ++ ident ++ "'"

bindIdent :: Ident -> Exp -> Env -> Env
bindIdent = Map.insert

eval :: Exp -> Evaluator Exp

--eval e@(App _ _) | trace (show e) False = undefined

--consOp = Var ":"

--eval e @ (Literal _) = return e
--eval e @ (App (App (Var ":") _head_) _tail_) = return e

eval (App fnExp argExp) = do
  fn <- strictEval fnExp
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

--eval (LetIn decls exp) = local (addManyToEnv decls) (eval exp)
eval (LetIn decls exp) = return $ foldr (\(ident, exp) -> substitue ident exp) exp subs
  where
    subs = expandDeclarations decls


eval (Case exp caseList) = do
    matched <- mapM tryCaseOption caseList
    case find isJust matched of
      (Just (Just e)) -> return e
      Nothing -> throwError "Non-exhaustive pattern matching"
  where
    tryCaseOption :: (Pattern, Exp) -> Evaluator (Maybe Exp)
    tryCaseOption (pattern, resExp) = do
      trans <- matchPattern pattern exp
      case trans of
        (Just fn) -> return $ Just $ fn resExp
        Nothing -> return Nothing

    matchPattern :: Pattern -> Exp -> Evaluator (Maybe (Exp -> Exp))
    matchPattern (PatternVar ident) exp = do
      e <- strictEval exp  ---XXXXXXX
      --let e = exp
      return $ Just $ substitue ident e

    matchPattern (PatternConst lit) exp = do
      e <- strictEval exp
      case e of
        (Literal lit') | lit == lit' -> return $ Just id
        other -> return Nothing

    matchPattern (PatternList [(PatternListTail ident)]) exp =  return $ Just $ substitue ident exp

    matchPattern (PatternList (patHead:patTail)) exp = do
      e <- strictEval exp
      case e of
        (Cons expHead expTail) -> do
          headMatch <- matchPattern patHead expHead
          tailMatch <- matchPattern (PatternList patTail) expTail
          return $ (.) <$> headMatch <*> tailMatch
        other -> return Nothing

    matchPattern (PatternList []) exp = do
      e <- strictEval exp
      case e of
        (Literal (LitSybmol "Nil")) -> return $ Just id
        other -> return Nothing


eval e = return e

strictEval :: Exp -> Evaluator Exp
strictEval exp = case exp of
  Lambda _ _   -> return exp
  Literal _    -> return exp
  InternalFn _ -> return exp
  Cons _ _     -> return exp
  otherwise    -> strictEval =<< eval exp


substitue :: Ident -> Exp -> Exp -> Exp
substitue ident target _ | trace (">>> " ++ ident ++ " ===>> " ++ (show target)) False = undefined

substitue ident target exp =
  case exp of
    Var id | id == ident -> target

    Lambda id body | id /= ident -> Lambda id (subs body)

    App exp1 exp2 -> App (subs exp1) (subs exp2)

    Cons exp1 exp2 -> Cons (subs exp1) (subs exp2)

    Case exp1 options -> let
        fn (p,e) = if varInPattern ident p
            then (p,e)
            else (p, subs e)
      in Case (subs exp1) (map fn options)

    LetIn decls exp -> let
        decls' = map substitueInDecl decls
        exp' = if any (\(Decl i _ _) -> i == ident) decls then exp else (subs exp)
      in LetIn decls' exp'

    other -> exp
  where
    subs = substitue ident target
    varInPattern :: Ident -> Pattern -> Bool
    varInPattern ident (PatternVar id) = id == ident
    varInPattern ident (PatternList list) = any (varInPattern ident) list
    varInPattern _ _ = False
    substitueInDecl d@(Decl i p e) | ident == i || (varInPattern ident p) = d
                                   | otherwise = Decl i p (subs e)





--
-- Primitives

primitives :: [(Ident, Exp -> Either String Exp)]
primitives =
  [ ("`+", binaryIntOp (+))
  , ("`-", binaryIntOp (-))
  , ("`*", binaryIntOp (*))
  , ("`/", binaryIntOp (div))
  , ("`==", binaryBoolOp (==))
  ]
    where
      binaryIntOp op (Literal (LitInt x)) = return $ InternalFn $ unaryIntOp (op x)
      binaryIntOp _ _ = integerError
      unaryIntOp op (Literal (LitInt x)) = return $ Literal $ LitInt (op x)
      unaryIntOp _ _ = integerError
      integerError = throwError "Integer function called on not a number"

      binaryBoolOp op (Literal lit) = return $ InternalFn $ unaryBoolOp (op lit)
      unaryBoolOp op (Literal lit) = return $ Literal $ LitSybmol (if (op lit) then "True" else "False")

internalFunctionsEnv :: Env
internalFunctionsEnv = Map.fromList $ map (\(id, fn) -> (id, InternalFn fn)) primitives


tst = App (Lambda "y" (App (Var "id") (Var "y"))) (Var "x")
tstEnv = Map.union internalFunctionsEnv $ Map.fromList [("id", Lambda "z" (Var "z")), ("x", Literal $ LitInt 42)]


