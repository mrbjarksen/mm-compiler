{-# LANGUAGE OverloadedStrings #-}

module Compiler.Masm (getMachineCode) where

import Compiler.Utils

import Control.Monad (forM_)
import Control.Monad.State

import Data.List (intersperse, group)

import qualified Data.Text as T
import Data.Text (Text, pack)

import Debug.Trace
import Data.Maybe (listToMaybe)

---- Morpho machine code ----

data Cmd = Call  Name Int
         | CallR Name Int
         | CallClosure  Pos
         | CallClosureR Pos
         | Drop Int
         | Fetch' Pos
         | FetchP Pos
         | FetchR Pos
         | Go'     Label
         | GoFalse Label
         | GoTrue  Label
         | MakeClosure  Label Int Int Label
         | MakeClosureP Label Int Int Label
         | MakeVal  String
         | MakeValP String
         | MakeValR String
         | Not'
         | NotP
         | NotR
         | Push
         | Return'
         | Store' Pos
         | StoreP Pos
         | StoreR Pos
         | List' Int
         | ListR Int
         | MkLabel Label

type CmdList = State (Label, [Cmd])
type Label = Int

consume :: Cmd -> CmdList ()
consume cmd = modify $ \(lab, cmds) -> (lab, add cmd cmds)

add :: Cmd -> [Cmd] -> [Cmd]
add Return' (Call name n         : cmds) = CallR name n         : cmds
add Return' (CallClosure pos     : cmds) = CallClosureR pos     : cmds
add Return' (Fetch' pos          : cmds) = FetchR pos           : cmds
add Return' (MakeVal lit         : cmds) = MakeValR lit         : cmds
add Return' (Not'                : cmds) = NotR                 : cmds
add Return' (Store' pos          : cmds) = StoreR pos           : cmds
add Return' (List' num           : cmds) = ListR num            : cmds
add (Fetch' pos)          (Push : cmds) = FetchP pos           : cmds
add (MakeClosure s n k e) (Push : cmds) = MakeClosureP s n k e : cmds
add (MakeVal lit)         (Push : cmds) = MakeValP lit         : cmds
add Not'                  (Push : cmds) = NotP                 : cmds
add (Store' pos)          (Push : cmds) = StoreP pos           : cmds
add Return' (Drop _ : cmds) = add Return' cmds
add cmd cmds = cmd : cmds

newLabel :: CmdList Label
newLabel = gets fst >>= \lab -> modify (\(a,b) -> (a+1,b)) >> return lab

withBetween :: Cmd -> (a -> CmdList ()) -> [a] -> CmdList ()
withBetween cmd f = void . sequence . intersperse (consume cmd) . map f

---- Conversion of AST elements to Morpho machine code ----

consumeProgram :: Program -> CmdList ()
consumeProgram (Program stmts) = do
    mapM_ consumeStmt $ stmts
    consume           $ Return'

consumeBody :: Body -> CmdList ()
consumeBody (Body stmts k) = do
    mapM_ consumeStmt $ stmts
    if k /= 0
       then consume   $ Drop k
       else return ()

consumeStmt :: Stmt -> CmdList ()
consumeStmt (FunDecl new pos n k body) = do
    startlab <- newLabel
    endlab   <- newLabel
    if new
       then do
        consume $ MakeVal "null"
        consume $ Push
       else return ()
    consume     $ MakeClosure startlab n k endlab
    consume     $ MkLabel startlab
    consumeBody $ body
    consume     $ Return'
    consume     $ MkLabel endlab
    consume     $ Store' pos
consumeStmt (VarDecl vds) = mapM_ consumeVarDecls $ group vds
consumeStmt (Expr expr) = consumeExpr expr

consumeVarDecls :: [VarDecl] -> CmdList ()
consumeVarDecls [VarDeclAndInit expr] = do
    consumeExpr $ expr
    consume     $ Push
consumeVarDecls noinits = do
    consume                         $ MakeVal "null"
    forM_ noinits . const . consume $ Push

consumeExpr :: Expr -> CmdList ()
consumeExpr (CallFun name args) = do
    withBetween Push consumeExpr $ args
    consume                      $ Call name (length args)
consumeExpr (CallCls expr args) = do
    consumeExpr                  $ expr
    consume                      $ Push
    withBetween Push consumeExpr $ args
    consume                      $ CallClosure (length args)
consumeExpr (Closure n k body) = do
    startlab <- newLabel
    endlab   <- newLabel
    consume     $ MakeClosure startlab n k endlab
    consume     $ MkLabel startlab
    consumeBody $ body
    consume     $ Return'
    consume     $ MkLabel endlab
consumeExpr (Fetch pos) = consume $ Fetch' pos
consumeExpr (Store pos expr) = do
    consumeExpr $ expr
    consume     $ Store' pos
consumeExpr (Literal lit) = consume $ MakeVal lit
consumeExpr (Number  num) = consume $ MakeVal num
consumeExpr (List []) = consume $ MakeVal "null"
consumeExpr (List exprs) = do
    withBetween Push consumeExpr $ exprs
    consume                      $ List' (length exprs)
consumeExpr (Return expr) = do
    consumeExpr $ expr
    consume     $ Return'
consumeExpr expr@(IfElse ic eics ec) = do
    labs <- replicateM (ifExprLength expr) newLabel
    let endlab = last labs
    forM_ (zip (ic:eics) labs) $ \(If cond body, lab) -> do
        consumeExpr $ cond
        consume     $ GoFalse lab
        consumeBody $ body
        if lab /= endlab
           then consume $ Go' endlab
           else return ()
        consume $ MkLabel lab
    case ec of
      NoElse    -> return ()
      Else body -> do
          consumeBody $ body
          consume     $ MkLabel endlab
consumeExpr (While cond body) = do
    startlab <- newLabel
    endlab   <- newLabel
    consume     $ MkLabel startlab
    consumeExpr $ cond
    consume     $ GoFalse endlab
    consumeBody $ body
    consume     $ Go' startlab
    consume     $ MkLabel endlab
consumeExpr (Or e1 e2) = do
    endlab <- newLabel
    consumeExpr $ e1
    consume     $ GoTrue endlab
    consumeExpr $ e2
    consume     $ MkLabel endlab
consumeExpr (And e1 e2) = do
    endlab <- newLabel
    consumeExpr $ e1
    consume     $ GoFalse endlab
    consumeExpr $ e2
    consume     $ MkLabel endlab
consumeExpr (Not expr) = do
    consumeExpr $ expr
    consume     $ Not'
consumeExpr (Go m body) = do
    startlab <- newLabel
    endlab   <- newLabel
    consume     $ MakeClosure startlab 0 m endlab
    consume     $ MkLabel startlab
    consumeBody $ body
    consume     $ Return'
    consume     $ MkLabel endlab
    consume     $ Call "startTask" 1

---- Misc ----

instance Eq VarDecl where
    VarDeclOnly == VarDeclOnly = True
    _ == _ = False

ifExprLength :: Expr -> Int
ifExprLength (IfElse ic eics NoElse) = 1 + length eics
ifExprLength (IfElse ic eics _     ) = 2 + length eics
ifExprLength _ = undefined

--- Printing of Morpho machine code ----

instance Show Cmd where
    show (Call  name n) = "(Call #\""  ++ name ++ "[f" ++ show n ++ "]\" " ++ show n ++ ")" 
    show (CallR name n) = "(CallR #\"" ++ name ++ "[f" ++ show n ++ "]\" " ++ show n ++ ")" 
    show (CallClosure  pos) = "(CallClosure "  ++ show pos ++ ")"
    show (CallClosureR pos) = "(CallClosureR " ++ show pos ++ ")"
    show (Drop k) = "(Drop " ++ show k ++ ")"
    show (Fetch' pos) = "(Fetch "  ++ show pos ++ ")"
    show (FetchP pos) = "(FetchP " ++ show pos ++ ")"
    show (FetchR pos) = "(FetchR " ++ show pos ++ ")"
    show (Go'     lab) = "(Go _"      ++ show lab ++ ")"
    show (GoFalse lab) = "(GoFalse _" ++ show lab ++ ")"
    show (GoTrue  lab) = "(GoTrue _"  ++ show lab ++ ")"
    show (MakeClosure  startlab n k endlab) =
        "(MakeClosure _"  ++ show startlab ++ " " ++ show n ++ " " ++ show k ++ " _" ++ show endlab ++ ")"
    show (MakeClosureP startlab n k endlab) =
        "(MakeClosureP _" ++ show startlab ++ " " ++ show n ++ " " ++ show k ++ " _" ++ show endlab ++ ")"
    show (MakeVal  lit) = "(MakeVal "  ++ lit ++ ")"
    show (MakeValP lit) = "(MakeValP " ++ lit ++ ")"
    show (MakeValR lit) = "(MakeValR " ++ lit ++ ")"
    show Not' = "(Not)"
    show NotP = "(NotP)"
    show NotR = "(NotR)"
    show Push = "(Push)"
    show Return' = "(Return)"
    show (Store' pos) = "(Store "  ++ show pos ++ ")"
    show (StoreP pos) = "(StoreP " ++ show pos ++ ")"
    show (StoreR pos) = "(StoreR " ++ show pos ++ ")"
    show (List' num) = "(List "  ++ show num ++ ")"
    show (ListR num) = "(ListR " ++ show num ++ ")"
    show (MkLabel lab) = "_" ++ show lab ++ ":"

getCmds :: Program -> [Cmd]
getCmds = reverse . snd . flip execState (0, []) . consumeProgram

getMachineCode :: String -> Program -> Text
getMachineCode name program = T.intercalate "\n" $
    [ T.concat ["\"", pack $ if name == "-" then "<stdout>" else name, "\" = main in"]
    , "!"
    , "{{"
    , "#\"main[f0]\" ="
    , "["
    ] ++ map (pack . show) (getCmds program) ++
    [ "];"
    , "}}"
    , "*"
    , "BASIS"
    , ";"
    ]
