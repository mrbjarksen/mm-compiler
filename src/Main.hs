module Main where

import Compiler.Lexer (getLexemes)
import Compiler.Parser (getSyntaxTree)
import Compiler.Code (getCode)
import Compiler.Utils

import System.Environment (getArgs)
import System.Exit (exitSuccess, exitFailure)

import qualified Data.Text.IO as TIO

import Control.Monad.State (runStateT, evalStateT)

import Data.List (partition)

chooseAction :: [String] -> IO ()
chooseAction args
    | "-h" `elem` args || "--help" `elem` args = showHelp
    | null files                               = failure "no input files"
    | opts == ["--show=tokens"]                = mapM_ showTokens files
    | opts == ["--show=ast"   ]                = mapM_ showAST    files
    | opts == ["--show=masm"  ] || null opts   = mapM_ showMASM   files
    | otherwise                                = failure "bad options"
    where (opts, files) = partition (startsWith '-') args
          startsWith x (c:_) = x == c

showHelp :: IO ()
showHelp = readFile "help.txt" >>= putStrLn >> exitSuccess

failure :: String -> IO ()
failure msg =  putStrLn ("mm-compiler: " ++ msg)
            >> putStrLn "Usage: For basic information, try the `--help' option."
            >> exitFailure

showTokens :: FilePath -> IO ()
showTokens fp = do
    result <- evalStateT getLexemes <$> compilerStart fp
    case result of
      Left err -> print err      >> exitFailure
      Right ls -> mapM_ print ls >> exitSuccess

showAST :: FilePath -> IO ()
showAST fp = do
    result <- runStateT getSyntaxTree <$> compilerStart fp
    case (\(a, s) -> (a, errors s)) <$> result of
      Left  err       -> print err                  >> exitFailure
      Right (ast, []) -> prettyPrint ast            >> exitSuccess
      Right (_, errs) -> mapM_ print (reverse errs) >> exitFailure

showMASM :: FilePath -> IO ()
showMASM fp = do
    result <- runStateT getSyntaxTree <$> compilerStart fp
    case (\(a, s) -> (a, errors s)) <$> result of
      Left  err       -> print err                     >> exitFailure
      Right (ast, []) -> TIO.putStrLn (getCode fp ast) >> exitSuccess
      Right (_, errs) -> mapM_ print  (reverse errs)   >> exitFailure

main :: IO ()
main = getArgs >>= chooseAction
