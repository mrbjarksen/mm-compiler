{
{-# LANGUAGE TupleSections #-}

module Compiler.Parser (getSyntaxTree) where

import Compiler.Lexer (getToken)
import Compiler.Utils

import Data.Text (empty)
import Data.Map (size)

import Control.Monad.State (get, gets, put)
import Control.Monad.Except (throwError)

}

%name getSyntaxTree

%tokentype { Token }

%error     { parseError }
%errorhandlertype explist

%monad { Compiler               }
%lexer { (getToken >>=) } { EOF }

%token
    'if'     { IF         }
    'else'   { ELSE       }
    'elsif'  { ELSIF      }
    'while'  { WHILE      }
    'var'    { VAR        }
    'fun'    { FUN        }
    'return' { RETURN     }
    'go'     { GO         }
    '||'     { OR         }
    '&&'     { AND        }
    '!'      { NOT        }
    OP1      { OP1     $$ }
    OP2      { OP2     $$ }
    OP3      { OP3     $$ }
    OP4      { OP4     $$ }
    OP5      { OP5     $$ }
    OP6      { OP6     $$ }
    OP7      { OP7     $$ }
    NAME     { NAME    $$ }
    LITERAL  { LITERAL $$ }
    '('      { DELIM '('  }
    ')'      { DELIM ')'  }
    '{'      { DELIM '{'  }
    '}'      { DELIM '}'  }
    ','      { DELIM ','  }
    ';'      { DELIM ';'  }
    '='      { DELIM '='  }

--%right '('
%left  '||'
%left  '&&'
%right '!'
%left  OP1
%right OP2
%left  OP3
%left  OP4
%left  OP5
%left  OP6
%left  OP7
%right UNARY

%%

---- Program ----

program :: { Program }
    : stmts { Program $1 }

stmts :: { [Stmt] }
    : stmt scs stmts { $1 : $3 }
    | stmt scs       { [$1]    }

stmt :: { Stmt }
    : fundecl     { $1      }
    | vardecl ';' { $1      }
    | expr ';'    { Expr $1 }

scs :: { () }
    : scs ';'     { () }
    | {- empty -} { () }

---- Body ----

names :: { [Name] }
    : names ',' NAME { $3 : $1 }
    | NAME           { [$1]    }
    | {- empty -}    { []      }

body :: { Body }
    : open stmts close { Body $2 $3 }

names_body :: { (Int, Body) }
    : names_open stmts close { ($1, Body $2 $3) }

open :: { () }
    : '{' {% pushScopeC }

names_open :: { Int }
    : '(' names ')' '{' {% pushScopeC >> mapM_ addVarC (reverse $2) >> return (length $2) }

close :: { Int }
    : '}' {% gets (size . curScope . scopeStack) <* popScopeC }

---- Declerations ----

fundecl :: { Stmt }
    : fun_NAME names_body   {% gets (numVars . scopeStack) >>= \num ->
                               return (FunDecl (fst $1) (fst $2) num (snd $2))
                            }
    | fun_OPNAME names_body {% reportErrorIf (fst $2 `notElem` [1,2]) (OpDefBadNumParams (snd $1) (fst $2)) 
                               >> gets (numVars . scopeStack) >>= \num -> 
                                  return (FunDecl (fst $1) (fst $2) num (snd $2))
                            }

fun_NAME :: { (Maybe Pos, Name) }
    : 'fun' NAME {% (,$2) `fmap` addFunC $2 }

fun_OPNAME :: { (Maybe Pos, Name) }
    : 'fun' OPNAME {% reportErrorIf ($2 `elem` ["||", "&&", "!"]) (CannotOverloadOp $2)
                      >> (,$2) `fmap` addFunC $2
                   }

vardecl :: { Stmt }
    : 'var' vardecls { VarDecl $2 }

vardecls :: { [VarDecl] }
    : declOrInit ',' vardecls { $1 : $3 }
    | declOrInit              { [$1]    }

declOrInit :: { VarDecl }
    : NAME          {% addVarC $1 >> return VarDeclOnly         }
    | NAME '=' expr {% addVarC $1 >> return (VarDeclAndInit $3) }

---- Expressions ----

expr :: { Expr }
    : 'return' expr { Return $2                                                   }
    | NAME '=' expr {% findVarC $1 >>= \pos -> return (Store pos $3)              }
    | 'go' body     {% gets (numVars . scopeStack) >>= \num -> return (Go num $2) }
    | opexpr        { $1                                                          }

OPNAME :: { String }
    : OP1 { $1 }
    | OP2 { $1 }
    | OP3 { $1 }
    | OP4 { $1 }
    | OP5 { $1 }
    | OP6 { $1 }
    | OP7 { $1 }

opexpr :: { Expr }
    : opexpr '||' opexpr      { Or  $1 $3           }
    | opexpr '&&' opexpr      { And $1 $3           }
    | '!' opexpr              { Not $2              }
    | opexpr OPNAME opexpr    {% mkCall $2 [$1, $3] }
    | OPNAME expr %prec UNARY {% mkCall $1 [$2]     }
    | smallexpr               { $1                  }

smallexpr :: { Expr }
    : NAME                      {% findVarC $1 >>= \pos -> return (Fetch pos) }
    | NAME '(' exprs ')'        {% mkCall $1 $3 }
    --| expr '(' exprs ')'        { CallCls $1 $3 }
    | LITERAL                   { Literal $1 }
    | '(' expr ')'              { $2 }
    | ifcase elseifs elsecase   { IfElse $1 $2 $3 }
    | 'while' '(' expr ')' body { While  $3 $5 }
    | 'fun' names_body          {% gets (numVars . scopeStack) >>= \num ->
                                   return (Closure num (fst $2) (snd $2))
                                }

ifcase :: { IfCase }
    : 'if' '(' expr ')' body { If $3 $5 }

elseifs :: { [ElseIfCase] }
    : 'elsif' '(' expr ')' body elseifs { (If $3 $5) : $6 }
    | {- empty -}                       { [] }

elsecase :: { ElseCase }
    : 'else' body { Else $2 }
    | {- empty -} { NoElse  }

exprs :: { [Expr] }
    : expr ',' exprs { $1 : $3 }
    | {- empty -}    { [] }

{

---- Parser utilities ----

pushScopeC :: Compiler ()
pushScopeC = get >>= \state ->
             put (state { scopeStack = pushScope $ scopeStack state })

popScopeC :: Compiler ()
popScopeC = get >>= \state ->
            put (state { scopeStack = popScope $ scopeStack state })

addVarC :: Name -> Compiler ()
addVarC name = do
    state <- get
    case addVar name $ scopeStack state of
      Left  _   -> reportError $ VarDefinedInScope name
      Right new -> put $ state { scopeStack = new }

addFunC :: Name -> Compiler (Maybe Int)
addFunC name = do
    state <- get
    case addVar name $ scopeStack state of 
      Left  pos -> return $ Just pos
      Right new -> put (state { scopeStack = new }) >> return Nothing

findVarC :: Name -> Compiler Int
findVarC name = do
    mpos <- gets $ findVar name . scopeStack 
    case mpos of
      Nothing  -> reportError (VarNotDefined name) >> return (-1)
      Just pos -> return pos
    
mkCall :: Name -> [Expr] -> Compiler Expr
mkCall name es = do
    mpos <- gets $ findVar name . scopeStack
    return $ case mpos of
      Nothing  -> CallFun name         es
      Just pos -> CallCls (Fetch pos)  es

---- Parse error handling ----

parseError :: (Token, [String]) -> Compiler a
parseError (tok, expected) = do
    state <- get
    put $ state { curBytes = [], inputText = empty }
    throwError $ GrammarError (symPos state) (ParseError tok expected)

}
