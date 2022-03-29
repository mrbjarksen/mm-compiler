module Main where

import Compiler.Lexer
import Compiler.Parser (getSyntaxTree)
import Compiler.Utils

import Data.List (partition)

import System.Environment (getArgs)
import System.Exit (exitSuccess, exitFailure)

import Control.Monad.State (evalStateT)

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
    result <- evalStateT getSyntaxTree <$> compilerStart fp
    case result of
      Left err  -> print err >> exitFailure
      Right ast -> print ast >> exitSuccess

showMASM :: FilePath -> IO ()
showMASM fp = undefined

main :: IO ()
main = getArgs >>= chooseAction
