module Evaluator where

import Prelude hiding (exp)

import Parser
import AST

import Control.Monad.Reader
import Control.Monad.Error
import qualified Data.Map as Map
import Data.List ( find, groupBy )
import Data.Maybe ( isJust, fromJust )
import Control.Applicative( (<$>), (<*>) )
import Data.IORef

--import Debug.Trace


type Env = Map.Map Ident Exp
type Evaluator = ErrorT String (ReaderT Env IO)

runEvaluator :: Evaluator Exp -> Env -> IO (Either String Exp)
runEvaluator evaluator env = runReaderT (runErrorT evaluator) env

addManyToEnv :: [Decl] -> Env -> Env
addManyToEnv decls env = Map.union (Map.fromList $ expandDeclarations decls) env


-- Expands set of declarations
-- It:
--   f 0 x = 1
--   f 1 2 = 3
--   f x y = ..
--   h = 0
-- will be transformed to:
--   f ↦ \x -> \y -> case [x y] of [0 x] -> 1; [1 2] -> 3; [x y] -> ..
--   h ↦ 0
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



returnIO :: IO a -> Evaluator a
returnIO = return =<< liftIO

eval :: Exp -> Evaluator Exp

eval (App fnExp argExp) = do
  fn <- strictEval fnExp
  case fn of
    (Lambda argName body) -> returnIO $ substitue argName argExp body
    (InternalFn fn) -> do
      evaled <- strictEval argExp
      case fn evaled of
        Right exp -> return exp
        Left err -> throwError err
    otherwise ->
      throwError ("Not a function " ++ (show fn))

eval (Var varName) = lookupIdent varName

eval (LetIn decls exp) = returnIO $ foldM (\ee (ident, exp) -> substitue ident exp ee) exp subs
  where
    subs = map applyFixPointOperator (expandDeclarations decls)
    applyFixPointOperator (ident, exp) = (ident, App (Var "fix") (Lambda ident exp))

eval (Case exp caseList) = do
    matched <- mapM tryCaseOption caseList
    case find isJust matched of
      (Just (Just e)) -> returnIO e
      Nothing -> throwError "Non-exhaustive pattern matching"
  where
    tryCaseOption :: (Pattern, Exp) -> Evaluator (Maybe (IO Exp))
    tryCaseOption (pattern, resExp) = do
      transformer <- matchPattern pattern exp
      case transformer of
        (Just fn) -> return . Just $ fn resExp
        Nothing -> return Nothing

    matchPattern :: Pattern -> Exp -> Evaluator (Maybe (Exp -> IO Exp))

    -- Var matchs to ant expression
    matchPattern (PatternVar ident) exp = return . Just $ substitue ident exp

    -- Constant value matchs to the same value
    matchPattern (PatternConst lit) exp = do
      e <- strictEval exp
      case e of
        (Literal lit') | lit == lit' -> return $ Just return
        other -> return Nothing

    matchPattern (PatternList [(PatternListTail ident)]) exp = return . Just $ substitue ident exp

    matchPattern (PatternList (patHead:patTail)) exp = do
      e <- strictEval exp
      case e of
        (Cons expHead expTail) -> do
          headMatch <- matchPattern patHead expHead
          tailMatch <- matchPattern (PatternList patTail) expTail
          return $ do
            h <- headMatch
            t <- tailMatch
            return $ t >=> h
        other -> return Nothing

    matchPattern (PatternList []) exp = do
      e <- strictEval exp
      case e of
        (Literal (LitSybmol "Nil")) -> return $ Just return
        other -> return Nothing

-- open Ref, eval inner expr, put evaled expr in Ref
eval (EIORef r) = do
  exp <- liftIO $ readIORef r
  evaled <- eval exp
  liftIO $ writeIORef r evaled
  return evaled

eval e = return e

strictEval :: Exp -> Evaluator Exp
strictEval exp = case exp of
  Lambda _ _   -> return exp
  Literal _    -> return exp
  InternalFn _ -> return exp
  Cons _ _     -> return exp
  otherwise    -> strictEval =<< eval exp


substitue :: Ident  -- change occurences of this var
          -> Exp    -- to this expression
          -> Exp    -- in this expression
          -> IO Exp

substitue ident target exp@(EIORef r) = do
  exp' <- liftIO $ readIORef r
  e <- substitue ident target exp'
  liftIO $ writeIORef r e  -- write or not?
  return e

substitue ident target@(EIORef _) exp =
  case exp of
    Var id | id == ident -> return target

    Lambda id body | id /= ident -> Lambda id <$> subs body

    App exp1 exp2 -> App <$> subs exp1 <*> subs exp2

    Cons exp1 exp2 -> Cons <$> subs exp1 <*> subs exp2

    Case exp1 options -> let
        fn (p,e) = do
          se <- subs e
          return $ if varInPattern ident p
            then (p,e)
            else (p, se)
      in Case <$> subs exp1 <*> mapM fn options

    LetIn decls exp -> let
        decls' = mapM substitueInDecl decls
        exp' = if any (\(Decl i _ _) -> i == ident) decls then (return exp) else (subs exp)
      in LetIn <$> decls' <*> exp'

    other -> return exp
  where
    subs = substitue ident target
    varInPattern :: Ident -> Pattern -> Bool
    varInPattern ident (PatternVar id) = id == ident
    varInPattern ident (PatternListTail id) = id == ident
    varInPattern ident (PatternList list) = any (varInPattern ident) list
    varInPattern _ _ = False
    substitueInDecl d@(Decl i p e) | ident == i || (varInPattern ident p) = return d
                                   | otherwise = do{ ne <- subs e; return $ Decl i p ne}


substitue ident target exp = do
  ref <- newIORef target
  substitue ident (EIORef ref) exp


