module Repl where

import Prelude hiding (exp)

import Parser
import Evaluator

import Text.ParserCombinators.Parsec

import System.Console.Haskeline
import Control.Monad.State
import Control.Applicative( (<$>) )

t1 = parse program "" "a=1; b=2; c = let q = 41 in q+a+b"


data Command
  = Declare Program
  | Quit
  | Eval Exp
  | LoadFile String
  deriving Show

parserInput :: Parser Command
parserInput = declareCmd <|>  evalCmd <|> (try  quitCmd) <|> loadCmd

declareCmd = do
  string "let"
  many space
  Declare <$> program

evalCmd = Eval <$> expr
quitCmd = do
  string ":q" <|> string ":quit"
  many space
  return Quit

loadCmd = do
  string ":l"
  many space
  LoadFile <$> (many anyChar)

processInput :: InputT (StateT Env IO) ()
processInput = do
  minput <- getInputLine "> "
  case minput of
    Nothing -> return ()
    Just "" -> processInput
    Just str -> do
      case parse parserInput "" str of
        Left err -> do
          outputStrLn $ show err
          processInput
        Right cmd -> do
          outputStrLn $ show cmd
          case cmd of

            Quit -> return ()

            Declare (Program decls) -> do
              env <- lift get
              lift $ put $ addManyToEnv decls env
              processInput

            Eval exp -> do
              env <- lift get
              outputStrLn $ show $ runEvaluator (strictEval exp) env
              processInput

            LoadFile file -> do
              f <- liftIO $ readFile file
              case parse program file f of
                Left err -> outputStrLn $ show err
                Right (Program decls) -> do
                  outputStrLn $ show decls
                  env <- lift get
                  lift $ put $ addManyToEnv decls env
              processInput


runRepl :: Env -> IO ()
runRepl env = evalStateT (runInputT defaultSettings processInput) env

main :: IO ()
main = do
  let env = createEnviroment (Program [])
  runRepl env

