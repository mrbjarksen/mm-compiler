module Compiler.Code where

import Compiler.Utils

import qualified Data.Text as T
import Data.Text (Text)
import qualified  Data.Text.IO as TIO

import Control.Monad.State
import Data.List (intercalate)

import System.FilePath.Posix (takeBaseName)

---- Definitions ----

type Cmd     = String
type CmdList = State Int [Cmd]
type Label   = String

---- Utility Funtions ----

sequenceC :: (a -> CmdList) -> [a] -> CmdList
sequenceC f = fmap concat . sequence . map f . reverse

sequenceC' :: Cmd -> (a -> CmdList) -> [a] -> CmdList
sequenceC' c f = fmap (intercalate [c]) . sequence . map f . reverse

infixl 1 <&
(<&) :: CmdList -> Cmd -> CmdList
cl <& c = (c:) <$> cl

infixl 1 &>
(&>) :: Cmd -> CmdList -> CmdList
c &> cl = (<>[c]) <$> cl

infixl 1 <&>
(<&>) :: CmdList -> CmdList -> CmdList
cl1 <&> cl2 = (<>) <$> cl2 <*> cl1

label :: State Int Label
label = get >>= \s -> modify (+1) >> return (show s)

len :: [a] -> String
len = show . length

---- Compilers ----

programC :: Name -> Program -> CmdList
programC name (Program stmts) = "\"" <> name <> ".mexe\" = main in\n!{{"
                             &> sequenceC stmtC stmts <& "}}*BASIS;"

bodyC :: Body -> CmdList
bodyC (Body stmts 0) = sequenceC stmtC stmts
bodyC (Body stmts m) = sequenceC stmtC stmts <& "(Drop " <> show m <> ")"

stmtC :: Stmt -> CmdList
stmtC (FunDecl Nothing k n body) = exprC (Closure k n body) <& "(Push)"
stmtC (FunDecl (Just pos) k n body) = exprC (Closure k n body) <& "(Store " <> show pos <> ")"
stmtC (VarDecl vds) = sequenceC vardeclC vds
stmtC (Expr expr) = exprC expr

vardeclC :: VarDecl -> CmdList
vardeclC VarDeclOnly = return ["(MakeVal null)"] <& "(Push)"
vardeclC (VarDeclAndInit expr) = exprC expr <& "(Push)"

exprC :: Expr -> CmdList
exprC (CallFun name es) = sequenceC' "(Push)" exprC es 
                        <& "(Call \"" <> name <> "[f" <> len es <> "]\" " <> len es <> ")"
exprC (CallCls expr es) = sequenceC' "(Push)" exprC (expr:es)
                        <& "(CallClosure " <> len es <> ")"
exprC (Closure k n b) = label >>= \lab -> "(MakeClosure 0 " <> show k <> " " <> show n <> " _" <> lab <> ")"
                        &> bodyC b <& "(Return)" <& "_" <> lab <> ":"
exprC (Fetch pos) = return $ ["(Fetch " <> show pos <> ")"]
exprC (Store pos expr) = exprC expr <& "(Store " <> show pos <> ")"
exprC (Literal lit) = return ["(MakeVal " <> lit <> ")"]
exprC (Return expr) = exprC expr <& "(Return)"
exprC (IfElse ic eics ec) = let cs = ic:eics
                            in  replicateM (length cs + fromEnum (ec /= NoElse)) label >>= \labs ->
                                 let end = last labs
                                 in  sequenceC id (zipWith (ifCaseC end) labs cs) <&> elseCaseC end ec
exprC (While expr body) = label >>= \startlab -> label >>= \endlab -> "_" <> startlab <> ":"
                        &> exprC expr <& "(GoFalse _" <> endlab <> ")"
                       <&> bodyC body <& "(Go _" <> startlab <> ")" <& "_" <> endlab <> ":"
exprC (Or e1 e2)  = label >>= \lab -> exprC e1 <& "(GoTrue _"  <> lab <> ")" <&> exprC e2 <& "_" <> lab <> ":"
exprC (And e1 e2) = label >>= \lab -> exprC e1 <& "(GoFalse _" <> lab <> ")" <&> exprC e2 <& "_" <> lab <> ":"
exprC (Not expr) = exprC expr <& "(Not)"
exprC (Go m body) = label >>= \lab -> "(MakeClosure 0 0 " <> show m <> " _" <> lab <> ")"
                  &> bodyC body <& "(Return)" <& "_" <> lab <> ":" <& "(Call #\"startTask[f1]\" 1)"

ifCaseC :: Label -> Label -> IfCase -> CmdList
ifCaseC end lab (If ie ib) = exprC ie <&  "(GoFalse _" <> lab <> ")"
                          <&> if lab == end
                                 then bodyC ib <&  "_" <> lab <> ":"
                                 else bodyC ib <& "(Go _" <> end <> ")" <& "_" <> lab <> ":"


elseCaseC :: Label -> ElseCase -> CmdList
elseCaseC _ NoElse = return []
elseCaseC end (Else eb) = bodyC eb <& "_" <> end <> ":"

instance Eq ElseCase where
    NoElse == NoElse = True
    _ == _ = False

---- Output ----

getCode :: FilePath -> Program -> Text
getCode fp = T.unlines . reverse . map T.pack . flip evalState 0 . programC (takeBaseName fp)

