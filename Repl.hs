module Main where

import Prelude hiding (exp)

import Parser
import Evaluator
import Printer

import Text.ParserCombinators.Parsec

import System.Console.Haskeline
import Control.Monad.State
import Control.Applicative( (<$>) )
import Data.List ( isPrefixOf )
import Data.Map ( keys )


data Command
  = Declare Program
  | Quit
  | Eval Exp
  | LoadFile String
  deriving Show

parserInput :: Parser Command
parserInput = do
  many space
  declareCmd <|>  evalCmd <|> (try  quitCmd) <|> loadCmd

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
  LoadFile <$> (many $ noneOf " \t")

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
              evalRes <- liftIO $ runEvaluator (strictEval exp) env
              case evalRes of
                (Right exp) -> outputStrLn $ pprint $ exp
                (Left err) -> outputStrLn $ "Error: " ++ err
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



--printResult :: Exp -> InputT (StateT Env IO) ()
--printResult exp


replSettings = Settings {
    historyFile = Just "repl-history",
    autoAddHistory = True,
    complete = let
      comp word = do
        env <- get
        files <- listFiles word
        return $ [simpleCompletion w | w <- (keys env), isPrefixOf word w] ++ files
      in
        completeWord Nothing " \t" comp
  }

runRepl :: Env -> IO ()
runRepl env = evalStateT (runInputT replSettings processInput) env

main :: IO ()
main = do
  let env = createEnviroment (Program [])
  runRepl env

