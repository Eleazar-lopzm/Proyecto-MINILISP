{
module Lexer (Token(..), lexer) where
import Data.Char (isSpace)
}

%wrapper "basic"

$white = [\x20\x09\x0A\x0D\x0C\x0B]
$letter = [A-Za-z]
$digit = [0-9]
$positive = [1-9]

tokens :-
    -- espacios en blanco y comentarios
    $white+               ;
    "--".*                ;

    -- signos de puntuacion
    "("                   { \_ -> TokenLPar }
    ")"                   { \_ -> TokenRPar }
    "["                   { \_ -> TokenLCor }
    "]"                   { \_ -> TokenRCor }
    ","                   { \_ -> TokenComa }

    -- operadores
    "<="                  { \_ -> TokenOp "<=" }
    ">="                  { \_ -> TokenOp ">=" }
    "!="                  { \_ -> TokenOp "!=" }
    "+"                   { \_ -> TokenOp "+" }
    "-"                   { \_ -> TokenOp "-" }
    "*"                   { \_ -> TokenOp "*" }
    "/"                   { \_ -> TokenOp "/" }
    "<"                   { \_ -> TokenOp "<" }
    ">"                   { \_ -> TokenOp ">" }
    "="                   { \_ -> TokenOp "=" }
    
    -- palabras reservadas
    "not"                 { \_ -> TokenReserv "not" }
    "letrec"              { \_ -> TokenReserv "letrec" }
    "let*"                { \_ -> TokenReserv "let*" }
    "let"                 { \_ -> TokenReserv "let" }
    "if0"                 { \_ -> TokenReserv "if0" }
    "if"                  { \_ -> TokenReserv "if" }
    "lambda"              { \_ -> TokenReserv "lambda" }
    "add1"                { \_ -> TokenReserv "add1" }
    "sub1"                { \_ -> TokenReserv "sub1" }
    "sqrt"                { \_ -> TokenReserv "sqrt" }
    "expt"                { \_ -> TokenReserv "expt" }
    "head"                { \_ -> TokenReserv "head" }
    "tail"                { \_ -> TokenReserv "tail" }
    "cond"                { \_ -> TokenReserv "cond" }
    "else"                { \_ -> TokenReserv "else" }
    "fst"                 { \_ -> TokenReserv "fst" }
    "snd"                 { \_ -> TokenReserv "snd" }

    -- literales booleanas
    "#t"                  { \_ -> TokenBool True }
    "#f"                  { \_ -> TokenBool False }

    -- enteros
    0                     { \_ -> TokenInt 0 }
    "-"$positive$digit*   { \s -> TokenInt (read s) }
    $positive$digit*      { \s -> TokenInt (read s) }

    -- identificadores de variables
    $letter+              { \s -> TokenVar s }

    -- por si lo ingresado no fue ninguno de los anteriores
    .                     { \s -> error ("Error lexico: caracter invalido = "
                                        ++ show s
                                        ++ " | codepoints = "
                                        ++ show (map fromEnum s)) }

{
data Token
    = TokenVar String
    | TokenInt Int
    | TokenBool Bool
    | TokenLPar
    | TokenRPar
    | TokenLCor
    | TokenRCor
    | TokenComa
    | TokenOp String
    | TokenReserv String
    deriving (Show, Eq)

-- para ejecutar el lexer
lexer :: String -> [Token]
lexer = alexScanTokens
}