{
module Compiler.Lexer (getToken, getLexemes) where

import Compiler.Utils

import qualified Data.Text as T
import Data.Text (Text)
import qualified Data.Text.IO as TIO

import Control.Monad.State (get, gets, put)
import Control.Monad.Except (throwError)

import Data.Char (ord)
import Data.Bits (shiftR, (.&.))

}

$digit   = [0-9]
$delim   = [\(\)\{\}\[\]\,\;\=]
$opelem  = [\*\/\%\+\-\<\>\!\=\&\|\:\?\~\^]

@int     = $digit+
@float   = $digit+\.$digit+([eE][\+\-]?$digit+)?
@strelem = \\[btnfr\"\'\\] | (\\[0-3][0-7][0-7]) | \\[0-7][0-7] | \\[0-7]
@string  = \"([^\"\\] | @strelem)*\"
@char    = \'([^\'\\] | @strelem)\'
@literal = @string | @char | true | false | null
@number  = @float | @int
@op1     = [\?\~\^]$opelem*
@op2     = \:$opelem*
@op3     = \|$opelem*
@op4     = \&$opelem*
@op5     = [\<\>\!\=]$opelem*
@op6     = [\+\-]$opelem*
@op7     = [\*\/\%]$opelem*
@name    = [a-zA-Z_][a-zA-Z0-9_]*

tokens :-

    <0,cmt> $white+  ;
    <0>     ";;;".*$ ;
    <0,cmt> "{;;;"   { const $ startComment >> getToken }
    <cmt>   ";;;}"   { const $ endComment   >> getToken }
    <cmt>   .        ;

    <0>     if       { mkTok  IF      }
    <0>     else     { mkTok  ELSE    }
    <0>     elsif    { mkTok  ELSIF   }
    <0>     while    { mkTok  WHILE   }
    <0>     var      { mkTok  VAR     }
    <0>     fun      { mkTok  FUN     }
    <0>     return   { mkTok  RETURN  }
    <0>     go       { mkTok  GO      }
    <0>     "||"     { mkTok  OR      }
    <0>     "&&"     { mkTok  AND     }
    <0>     "!"      { mkTok  NOT     }
    <0>     $delim   { mkTokC DELIM   }
    <0>     @literal { mkTokS LITERAL }
    <0>     @number  { mkTokS NUMBER  }
    <0>     @op1     { mkTokS OP1     }
    <0>     @op2     { mkTokS OP2     }
    <0>     @op3     { mkTokS OP3     }
    <0>     @op4     { mkTokS OP4     }
    <0>     @op5     { mkTokS OP5     }
    <0>     @op6     { mkTokS OP6     }
    <0>     @op7     { mkTokS OP7     }
    <0>     @name    { mkTokS NAME    }

{

---- Core (variations of functions provided by Alex) ----

type AlexInput = (FilePos, [Byte], Text)

toInput :: CState -> AlexInput
toInput (CState pos bs txt _ _ _ _) = (pos, bs, txt)

setInput :: AlexInput -> Compiler ()
setInput (pos, bs, txt) = get >>= \state ->
    put $ state { filePos = pos, curBytes = bs, inputText = txt }

alexGetByte :: AlexInput -> Maybe (Byte, AlexInput)
alexGetByte (pos, (b:bs), txt) = Just (b, (pos, bs, txt))
alexGetByte (pos, [],     txt) =
    case T.uncons txt of 
      Nothing        -> Nothing
      Just (c, txt') -> alexGetByte (nextPos pos c, utf8Encode c, txt')

alexInputPrevChar :: AlexInput -> Char
alexInputPrevChar = undefined

ignorePendingBytes :: AlexInput -> AlexInput
ignorePendingBytes (pos, _, txt) = (pos, [], txt)

utf8Encode :: Char -> [Byte]
utf8Encode = uncurry (:) . utf8Encode'

utf8Encode' :: Char -> (Byte, [Byte])
utf8Encode' c = case go (ord c) of (x, xs) -> (fromIntegral x, map fromIntegral xs)
    where go oc
            | oc <= 0x7f   = (oc, [])
            | oc <= 0x7ff  = (0xc0 + (oc `shiftR` 6), [0x80 + oc .&. 0x3f])
            | oc <= 0xffff = (0xe0 + (oc `shiftR` 12), [0x80 + ((oc `shiftR` 6) .&. 0x3f), 0x80 + oc .&. 0x3f])
            | otherwise    = ( 0xf0 + (oc `shiftR` 18)
                             , [ 0x80 + ((oc `shiftR` 12) .&. 0x3f)
                               , 0x80 + ((oc `shiftR` 6) .&. 0x3f)
                               , 0x80 + oc .&. 0x3f
                               ]
                             )

---- Token constructors and getter ----

mkTok :: Token -> String -> Compiler Token
mkTok = const . return

mkTokC :: (Char -> Token) -> String -> Compiler Token
mkTokC tok = return . tok . head

mkTokS :: (String -> Token) -> String -> Compiler Token
mkTokS = (return.)

getToken :: Compiler Token
getToken = get >>= \state@(CState pos _ txt cd _ _ _) ->
    case alexScan (toInput state) (min 1 cd) of
      AlexEOF               -> return EOF
      AlexError inp         -> throwError (GrammarError pos LexicalError) >> return EOF
      AlexSkip  inp len     -> setInput inp >> getToken
      AlexToken inp len act -> setInput inp >> setSymPos pos >> act (T.unpack $ T.take len txt)

---- Lexemes ----

data Lexeme = Lexeme FilePos Token String

getLexemes :: Compiler [Lexeme]
getLexemes = do
    tok <- getToken
    pos <- gets symPos
    let lexeme = Lexeme pos tok (toLexeme tok)
    case tok of
      EOF -> return []
      _   -> (lexeme:) <$> getLexemes

instance Show Lexeme where
    show (Lexeme pos tok str) = show pos ++ " Token: " ++ show tok ++ ", Lexeme: " ++ str

instance Show Token where
    show IF          = "IF"
    show ELSE        = "ELSE"
    show ELSIF       = "ELSIF"
    show WHILE       = "WHILE"
    show VAR         = "VAR"
    show FUN         = "FUN"
    show RETURN      = "RETURN"
    show GO          = "GO"
    show OR          = "OR"
    show AND         = "AND"
    show NOT         = "NOT"
    show (OP1 _)     = "OP1"
    show (OP2 _)     = "OP2"
    show (OP3 _)     = "OP3"
    show (OP4 _)     = "OP4"
    show (OP5 _)     = "OP5"
    show (OP6 _)     = "OP6"
    show (OP7 _)     = "OP7"
    show (NAME _)    = "NAME"
    show (LITERAL _) = "LITERAL"
    show (NUMBER _)  = "NUMBER"
    show (DELIM d)   = [d]
    show EOF         = "<EOF>"

}
