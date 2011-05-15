module Main where

import Prelude hiding (exp)

import AST
import Parser
import Evaluator
import Primitives
import Printer

import System.Environment


main = do
    files <- getArgs
    decls <- mapM loadProgramFile files
    case sequence decls of
      Nothing -> return ()
      (Just decls) -> do
        let env = createEnviroment $ Program $ concat decls
        input <- getContents
        let evaluator = strictEval (App (Var "show") (App (Var "main") (stringToYList input)))
        result <- runEvaluator evaluator env
        case result of
          (Right output) -> putStrLn =<< pprintListContent output
          (Left err) -> putStrLn $ "Error:" ++ (show err)

  where
    loadProgramFile :: FilePath -> IO (Maybe [Decl])
    loadProgramFile f = do
      p <- readFile f
      case runYParser program f p of
        Left err -> do
          putStrLn $ "Error:" ++ (show err)
          return Nothing
        Right (Program decls) -> return $ Just decls

    stringToYList str = foldr (\x -> Cons (Literal $ LitChar x)) (Literal $ LitSybmol "Nil") str

