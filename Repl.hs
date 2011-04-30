module Repl where

import Prelude hiding (exp)

import Parser
import Evaluator

import Text.ParserCombinators.Parsec

import System.Console.Haskeline
import Control.Monad.State
import Control.Applicative( (<$>) )

t1 = parse program "" "a=1; b=2; c = let q = 41 in q+a+b"


data Command = Declare Decl | Quit | Eval Exp deriving Show
parserInput :: Parser Command
parserInput = declareCmd <|> quitCmd <|> evalCmd

declareCmd = do
  string "let"
  many space
  Declare <$> declaration

evalCmd = Eval <$> expr
quitCmd = do
  string ":q" <|> string ":quit"
  many space
  return Quit

processInput :: InputT (StateT Env IO) ()
processInput = do
  minput <- getInputLine "> "
  case minput of
    Nothing -> return ()
    Just "" -> processInput
    Just str -> do
      case parse parserInput "" str of
        Left err -> outputStrLn $ show err
        Right cmd -> do
          outputStrLn $ show cmd
          case cmd of
            Quit -> return ()
            Declare decl -> do
              env <- lift get
              lift $ put $ addToEnv env decl
            Eval exp -> do
              env <- lift get
              outputStrLn $ show $ runEvaluator (strictEval exp) env
      processInput


runRepl :: Env -> IO ()
runRepl env = evalStateT (runInputT defaultSettings processInput) env

main :: IO ()
main = do
  let env = createEnviroment (Program [])
  runRepl env

