module Main (main) where

import Compiler.Lexer (getLexemes)
import Compiler.Parser (getSyntaxTree)
import Compiler.Masm (getMachineCode)
import Compiler.Utils

import System.Environment (getArgs)
import System.Exit (exitSuccess, exitFailure)
import System.FilePath ((-<.>), (<.>), dropExtension, isExtensionOf)
import System.Directory (doesFileExist)
import System.Process (callCommand)
import System.IO (Handle, IOMode(..), stdin, stdout, openFile, hPutStr, hPrint, hClose)
import GHC.IO.Handle (hDuplicate)

import Paths_mm_compiler (getDataFileName)

import Data.Text (Text)
import qualified Data.Text.IO as TIO

import Control.Monad (foldM, unless)
import Control.Monad.State (runStateT, evalStateT)

import Data.List (break)

---- Argument handling ----

data OutputType = Tokens | SyntaxTree | MachineCode | Executable deriving (Eq, Show)

data Arguments = Arguments
    { help        :: Bool
    , info        :: Bool
    , inputFiles  :: [FilePath]
    , outputFiles :: [FilePath]
    , outputType  :: OutputType
    , optsDone    :: Bool
    }
    deriving Show

defaultArgs :: Arguments
defaultArgs = Arguments False False [] [] Executable False

setHelp :: Bool -> Arguments -> Arguments
setHelp val args = args { help = val }

setInfo :: Bool -> Arguments -> Arguments
setInfo val args = args { info = val }

addInput :: FilePath -> Arguments -> Arguments
addInput file args = args { inputFiles = file : inputFiles args }

addOutputs :: [FilePath] -> Arguments -> Arguments
addOutputs files args = args { outputFiles = foldr (:) (outputFiles args) $ reverse files }

setOutputType :: OutputType -> Arguments -> Arguments
setOutputType ot args = args { outputType = ot }

setOptsDone :: Bool -> Arguments -> Arguments
setOptsDone val args = args { optsDone = val }

---- Argument parsing ----

refactorArgs :: [String] -> [(String, String)]
refactorArgs = joinArgs . break (=="--")
    where joinArgs (opts, noopts) = refactorArgs' opts ++ map emptyArgArg noopts
          emptyArgArg arg = (arg, "")

refactorArgs' :: [String] -> [(String, String)]
refactorArgs' = map getArgArg . foldr addToArgList []
    where addToArgList "-o" (a:as) = ("-o"++a) : as
          addToArgList "-s" (a:as) = ("-s"++a) : as
          addToArgList a as        = a : as
          getArgArg ('-':'o':argarg) = ("-o", argarg)
          getArgArg ('-':'s':argarg) = ("-s", argarg)
          getArgArg arg@('-':'-':_)  = removeEq $ break (=='=') arg
          getArgArg arg              = (arg, "")
          removeEq (a, '=':b) = (a, b)
          removeEq a = a

parseArg :: Arguments -> (String, String) -> Either String Arguments
parseArg args (arg, argarg)
    | arg == "--"                      = Right $ setOptsDone True args
    | matchOpts ["-h", "-?", "--help"] = Right $ setHelp True args
    | matchOpts ["-i", "--info"  ]     = Right $ setInfo True args
    | matchOpts ["-o", "--output"]     = addOutputsIfValid argarg
    | matchOpts ["-s", "--show"  ]     = setOutputTypeIfValid argarg
    | matchAnyOpt                      = Left $ "unknown option `" ++ arg ++ "'"
    | otherwise                        = Right $ addInput arg args
    where matchOpts opts = not (optsDone args) && arg `elem` opts
          matchAnyOpt    = not (optsDone args) && head arg == '-' && length arg > 1
          addOutputsIfValid "" = Left $ "no argument given to option `-o'/`--output'"
          addOutputsIfValid fs = let files = split ',' fs in
            if any null files
               then Left  $ "empty file name given to option `-o'/`--output'"
               else Right $ addOutputs files args
          split c = foldr (\x xs@(h:t) -> if x == c then []:xs else (x:h):t) [[]] 
          setOutputTypeIfValid st =
            case (st, outputType args) of
              ("tokens", Executable) -> Right $ setOutputType Tokens      args
              ("ast",    Executable) -> Right $ setOutputType SyntaxTree  args
              ("masm",   Executable) -> Right $ setOutputType MachineCode args
              ("",       Executable) -> Left  $ "no argument given to option `-s'/`--show'"
              (_,        Executable) -> Left  $ "invalid argument `" ++ st ++ "' given to option `-s'/`--show'" 
              _                      -> Left  $ "ambiguous use of option `-s'/`--show'"

parseArgs :: [String] -> Either String Arguments
parseArgs = foldM parseArg defaultArgs . refactorArgs

---- Program actions ----

chooseAction :: Either String Arguments -> IO ()
chooseAction (Left msg) = failure msg
chooseAction (Right args)
    | help args              = showHelp
    | info args              = showInfo
    | null $ inputFiles args = failure "no input files"
    | otherwise              = mapM_ outputFunc inoutPairs >> exitSuccess
    where inouts (i:ins) (o:outs) = (i, o)           : inouts ins outs
          inouts (i:ins) []       = (i, defOutput i) : inouts ins []
          inouts []      _        = []
          inoutPairs = inouts (reverse $ inputFiles args) (reverse $ outputFiles args)
          defOutput i = if outputType args == Executable then inputName i -<.> "mexe" else "-"
          outputFunc =
            case outputType args of 
              Tokens      -> outputTokens
              SyntaxTree  -> outputSyntaxTree
              MachineCode -> outputMachineCode
              Executable  -> outputExecutable

failure :: String -> IO ()
failure msg = mapM_ putStrLn
    [ "mm-compiler: " ++ msg
    , "Usage: For basic information, try the `--help' option."
    ] >> exitFailure

showHelp :: IO ()
showHelp = getDataFileName "help.txt" >>= TIO.readFile >>= TIO.putStrLn >> exitSuccess

showInfo :: IO ()
showInfo = getDataFileName "Parser.info" >>= TIO.readFile >>= TIO.putStrLn >> exitSuccess

outputTokens :: (FilePath, FilePath) -> IO ()
outputTokens (input, output) = do
    ih <- inputHandle input
    result <- evalStateT getLexemes <$> compilerStart (inputName input) ih
    hClose ih
    case result of
      Left err -> print err >> exitFailure
      Right ls -> do
          oh <- outputHandle output
          mapM_ (hPrint oh) ls
          hClose oh

withSyntaxTree :: FilePath -> (Program -> IO ()) -> IO ()
withSyntaxTree input cont = do
    ih <- inputHandle input
    result <- runStateT getSyntaxTree <$> compilerStart (inputName input) ih
    hClose ih
    case (\(a, s) -> (a, errors s)) <$> result of
      Left  err       -> print err                  >> exitFailure
      Right (ast, []) -> cont ast
      Right (_, errs) -> mapM_ print (reverse errs) >> exitFailure

outputSyntaxTree :: (FilePath, FilePath) -> IO ()
outputSyntaxTree (input, output) = withSyntaxTree input $ \ast -> do
    oh <- outputHandle output
    let indent = if output == "-" then "\ESC[38;5;240m|\ESC[0m " else "| "
    hPutStr oh $ programToString ast indent
    hClose oh

withMachineCode :: (FilePath, FilePath) -> (Text -> IO ()) -> IO ()
withMachineCode (input, output) cont = withSyntaxTree input $ cont . getMachineCode output

outputMachineCode :: (FilePath, FilePath) -> IO ()
outputMachineCode a@(input, output) = withMachineCode a $ \txt -> do
    oh <- outputHandle output
    TIO.hPutStrLn oh txt
    hClose oh

outputExecutable :: (FilePath, FilePath) -> IO ()
outputExecutable a@(input, output) = do
    if output == "-"
       then failure $ "cannot output executable to standard output" 
       else withMachineCode a $ \txt -> do
              oh <- outputHandle output
              TIO.hPutStrLn oh txt
              hClose oh
              morpho <- getDataFileName "morpho.jar"
              callCommand $ "java -jar " ++ morpho ++ " -c \"" ++ output ++ "\""

ifExists :: FilePath -> IO a -> IO a
ifExists file cont = do
    exists <- doesFileExist file
    unless exists $ failure ("could not find file " ++ file)
    cont

inputName :: FilePath -> String
inputName "-" = "<stdin>"
inputName fp  = fp

inputHandle :: FilePath -> IO Handle
inputHandle "-"   = hDuplicate stdin
inputHandle file  = ifExists file $ openFile file ReadMode 

outputHandle :: FilePath -> IO Handle
outputHandle "-"  = hDuplicate stdout
outputHandle file = openFile file WriteMode

runPrograms :: [String] -> IO ()
runPrograms args
    | "-h"     `elem` args = showHelp
    | "--help" `elem` args = showHelp
    | any isOption args    = failure $ "run mode takes no options"
    | null args            = failure $ "no input files"
    | otherwise            = mapM_ run args
    where isOption "-"     = False
          isOption ('-':_) = True
          isOption _       = False

run :: FilePath -> IO ()
run "-" = failure $ "cannot run executable from standard input"
run fp  = let file = if "mexe" `isExtensionOf` fp then fp else fp <.> "mexe" in do
    morpho <- getDataFileName "morpho.jar"
    ifExists file $ callCommand $ "java -jar " ++ morpho ++ " \"" ++ dropExtension file ++ "\""

---- main ----

main :: IO ()
main = getArgs >>= runOrCompile
    where runOrCompile ("run":args) = runPrograms args
          runOrCompile args         = chooseAction $ parseArgs args
