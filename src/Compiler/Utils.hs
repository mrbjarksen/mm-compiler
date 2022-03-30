module Compiler.Utils where

import Data.Text (Text)
import qualified Data.Text.IO as TIO
import Data.Word (Word8)

import Control.Monad.State (StateT, get, gets, put)

import Text.Parsec

import qualified Data.Map as M
import Data.Maybe (listToMaybe, mapMaybe)
import Data.List (intersperse)

---- Aliases for readability ----

type Name     = String
type OpName   = String
type Pos      = Int
type LineNum  = Int
type ColNum   = Int
type Byte     = Word8

---- Tokens to be returned by lexer ----

data Token = IF
           | ELSE
           | ELSIF
           | WHILE
           | VAR
           | FUN
           | RETURN
           | GO
           | OR
           | AND
           | NOT
           | OP1     OpName
           | OP2     OpName
           | OP3     OpName
           | OP4     OpName
           | OP5     OpName
           | OP6     OpName
           | OP7     OpName
           | NAME    Name
           | LITERAL String
           | DELIM   Char
           | EOF

toLexeme :: Token -> String
toLexeme IF            = "if"
toLexeme ELSE          = "else"
toLexeme ELSIF         = "elsif"
toLexeme WHILE         = "while"
toLexeme VAR           = "var"
toLexeme FUN           = "fun"
toLexeme RETURN        = "return"
toLexeme GO            = "go"
toLexeme OR            = "||"
toLexeme AND           = "&&"
toLexeme NOT           = "!"
toLexeme (OP1 op)      = op
toLexeme (OP2 op)      = op
toLexeme (OP3 op)      = op
toLexeme (OP4 op)      = op
toLexeme (OP5 op)      = op
toLexeme (OP6 op)      = op
toLexeme (OP7 op)      = op
toLexeme (NAME name)   = name
toLexeme (LITERAL lit) = lit
toLexeme (DELIM d)     = [d]
toLexeme EOF           = "<EOF>"

---- Elements of AST returned by parser ----

newtype Program = Program [Stmt]

data Body = Body [Stmt] Int

data Stmt = FunDecl (Maybe Pos) Int Int Body
          | VarDecl [VarDecl]
          | Expr    Expr

data VarDecl = VarDeclOnly
             | VarDeclAndInit Expr

data Expr = CallFun  Name    [Expr]
          | CallCls  Expr    [Expr]
          | Closure  Int     Int          Body
          | Fetch    Pos
          | Store    Pos     Expr
          | Literal  String
          | Return   Expr
          | IfElse   IfCase  [ElseIfCase] ElseCase
          | While    Expr    Body
          | Or       Expr    Expr
          | And      Expr    Expr
          | Not      Expr
          | Go       Int     Body

data IfCase     = If     Expr Body
type ElseIfCase = IfCase
data ElseCase   = Else   Body
                | NoElse

prettyPrint :: Program -> IO ()
prettyPrint ast = do
    case runParser prettify 0 "" $ show ast of
      Left  _      -> print ast
      Right pretty -> putStrLn pretty

indentString :: String
indentString = "\ESC[38;5;240m|\ESC[0m "

prettify :: Parsec String Int String
prettify = fmap (rmempty . concat) $
    many  $  char ' ' *> newline
         <|> char '(' *> nest 1  *> newline
         <|> char ')' *> nest (-1) *> newline
         <|> (try $ string "[]")
         <|> char '[' *> pure "[ " <* nest 1
         <|> char ',' *> nest (-1) *> newline <> pure ", " <* nest 1
         <|> char ']' *> nest (-1) *> newline <> pure "]" <> newline
         <|> (\x -> [x]) <$> anyChar
    where newline = ('\n':) . concat <$> (replicate <$> getState <*> pure indentString)
          nest n = modifyState (+n)
          rmempty  = unlines . filter (not . rmempty') . lines
          rmempty' s = null s || last s == last indentString

instance Show Program where
    show (Program stmts) = "Program (" ++ show stmts ++ ")"

instance Show Body where
    show (Body stmts k) = "Body (" ++ show stmts ++ ") (" ++ show k ++ ")"

instance Show Stmt where
    show (FunDecl mp m n b) = "FunDecl (" ++ show mp  ++ ") (" ++ show m ++ ") (" 
                                          ++ show n   ++ ") (" ++ show b ++ ")"
    show (VarDecl vds     ) = "VarDecl (" ++ show vds ++ ")"
    show (Expr    e       ) = "Expr ("    ++ show e   ++ ")"

instance Show VarDecl where
    show VarDeclOnly        = "VarDeclOnly"
    show (VarDeclAndInit e) = "VarDeclAndInit (" ++ show e ++ ")"

instance Show Expr where
    show (CallFun  n  es    ) = "CallFun ("  ++      n  ++ ") (" ++ show es  ++ ")"
    show (CallCls  e  es    ) = "CallCls ("  ++ show e  ++ ") (" ++ show es  ++ ")"
    show (Closure  nv na  b ) = "Closure ("  ++ show nv ++ ") (" ++ show na  ++ ") (" ++ show b  ++ ")"
    show (Fetch    p        ) = "Fetch ("    ++ show p  ++ ")"
    show (Store    p  e     ) = "Store ("    ++ show p  ++ ") (" ++ show e   ++ ")"
    show (Literal  l        ) = "Literal ("  ++ show l  ++ ")"
    show (Return   e        ) = "Return ("   ++ show e  ++ ")"
    show (IfElse   i  eis e ) = "IfElse ("   ++ show i  ++ ") (" ++ show eis ++ ") (" ++ show e  ++ ")"
    show (While    e  b     ) = "While ("    ++ show e  ++ ") (" ++ show b   ++ ")"
    show (Or       e1 e2    ) = "Or ("       ++ show e1 ++ ") (" ++ show e2  ++ ")"
    show (And      e1 e2    ) = "And ("      ++ show e1 ++ ") (" ++ show e2  ++ ")"
    show (Not      e        ) = "Not ("      ++ show e  ++ ")"
    show (Go       m  b     ) = "Go ("       ++ show m  ++ ") (" ++ show b   ++ ")"

instance Show IfCase where
    show (If e b) = "If (" ++ show e ++ ") (" ++ show b ++ ")"

instance Show ElseCase where
    show (Else b) = "Else (" ++ show b ++ ")"
    show NoElse   = "NoElse"

---- Position in file ----

data FilePos = FilePos FilePath LineNum ColNum

nextPos :: FilePos -> Char -> FilePos
nextPos (FilePos fp l c) '\n' = FilePos fp (l+1) 1
nextPos (FilePos fp l c) _    = FilePos fp l     (c+1)

instance Show FilePos where
    show (FilePos fp l c) = fp ++ ":" ++ show l ++ ":" ++ show c

---- Scope stack ----

type Scope = M.Map String Int
data ScopeStack = ScopeStack { numVars :: Int, curScope :: Scope, prevScopes :: [Scope] }

emptyScopeStack :: ScopeStack
emptyScopeStack = ScopeStack 0 M.empty []

scopes :: ScopeStack -> [Scope]
scopes s = curScope s : prevScopes s

pushScope :: ScopeStack -> ScopeStack
pushScope (ScopeStack n cur prevs) = ScopeStack n M.empty (cur:prevs)

popScope :: ScopeStack -> ScopeStack
popScope (ScopeStack _ cur [])     = emptyScopeStack
popScope (ScopeStack n cur (p:ps)) = ScopeStack (n-k) p ps
    where k = M.size cur

addVar :: Name -> ScopeStack -> Either Pos ScopeStack
addVar name (ScopeStack n cur prevs) =
    case M.lookup name cur of
      Nothing  -> Right $ ScopeStack (n+1) (M.insert name n cur) prevs
      Just pos -> Left pos

findVar :: Name -> ScopeStack -> Maybe Int
findVar name = listToMaybe . mapMaybe (M.lookup name) . scopes

---- State of compiler while file is being read ----

data CState = CState { filePos      :: FilePos         -- Position in file
                     , curBytes     :: [Byte]          -- Remaining bytes of current character
                     , inputText    :: Text            -- Remaining input text
                     , commentDepth :: Int             -- Depth of current block comment
                     , scopeStack   :: ScopeStack      -- Currently defined variables
                     , symPos       :: FilePos         -- Position of grammar symbol on top of stack
                     , errors       :: [SemanticError] -- Semantic errors encountered
                     }

compilerStart :: FilePath -> IO CState
compilerStart fp = do
    txt <- TIO.readFile fp
    return $ CState { filePos      = FilePos fp 1 1
                    , curBytes     = []
                    , inputText    = txt
                    , commentDepth = 0
                    , scopeStack   = emptyScopeStack
                    , symPos       = FilePos "<unknown>" 0 0
                    , errors       = []
                    }

---- Monad to thread throught lexer and parser ----

type Compiler = StateT CState (Either GrammarError)

startComment :: Compiler ()
startComment = get >>= \state ->
               put $ state { commentDepth = commentDepth state + 1 }

endComment :: Compiler ()
endComment = get >>= \state ->
             put $ state { commentDepth = min 0 (commentDepth state - 1) }

setSymPos :: FilePos -> Compiler ()
setSymPos pos = get >>= \state ->
                put $ state { symPos = pos }

getNumVars :: Compiler Int
getNumVars = gets (numVars . scopeStack)

---- Error handling ----

data GErrType = LexicalError
              | ParseError Token [String]

data SErrType = VarNotDefined     Name
              | VarDefinedInScope Name
              | CannotOverloadOp  OpName
              | OpDefBadNumParams OpName Int

data GrammarError  = GrammarError  FilePos GErrType
data SemanticError = SemanticError FilePos SErrType

reportError :: SErrType -> Compiler ()
reportError err = get >>= \state ->
                  put $ state { errors = (SemanticError (symPos state) err) : errors state }

reportErrorIf :: Bool -> SErrType -> Compiler ()
reportErrorIf cond err = if cond then reportError err else return ()

instance Show GErrType where
    show LexicalError         = "lexical error"
    show (ParseError tok exp) = "parse error on input `" ++ toLexeme tok ++ "'\n\t"
                                ++ "expected: " ++ concat (intersperse ", " exp)

instance Show SErrType where
    show (VarNotDefined     name) = "variable `" ++ name ++ "' is not defined"
    show (VarDefinedInScope name) = "variable `" ++ name ++ "' is already defined in current scope"
    show (CannotOverloadOp  op  ) = "cannot overload operator `" ++ op ++ "'"
    show (OpDefBadNumParams op n) = "operator `" ++ op ++ "' cannot have " ++ show n
                                    ++ "parameters (must be 1 or 2)"

instance Show GrammarError where
    show (GrammarError fp err) = show fp ++ "\n\t" ++ show err

instance Show SemanticError where
    show (SemanticError fp err) = show fp ++ "\n\t" ++ show err
