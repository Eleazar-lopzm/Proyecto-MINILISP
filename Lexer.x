{
module Lexer (Token(..), lexer) where
import Data.Char (isSpace)
}

%wrapper "basic"

$white = [\x20\x09\x0A\x0D\x0C\x0B]
$letter = [A-Za-z]
$digit = [0-9]
$positive = [1-9]
$id_rest = [$letter$digit\-\_]

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
    
    -- palabras reservadas -> tokens específicos
    "not"                 { \_ -> TokenNot }
    "letrec"              { \_ -> TokenLetRec }
    "let*"                { \_ -> TokenLetStar }
    "let"                 { \_ -> TokenLet }
    "if0"                 { \_ -> TokenIf0 }
    "if"                  { \_ -> TokenIf }
    "lambda"              { \_ -> TokenLambda }
    "add1"                { \_ -> TokenAdd1 }
    "sub1"                { \_ -> TokenSub1 }
    "sqrt"                { \_ -> TokenSqrt }
    "expt"                { \_ -> TokenExpt }
    "head"                { \_ -> TokenHead }
    "tail"                { \_ -> TokenTail }
    "cond"                { \_ -> TokenCond }
    "else"                { \_ -> TokenElse }
    "fst"                 { \_ -> TokenFst }
    "snd"                 { \_ -> TokenSnd }
    "pair"                { \_ -> TokenPair }


    -- literales booleanas
    "#t"                  { \_ -> TokenBool True }
    "#f"                  { \_ -> TokenBool False }

    -- enteros
    0                     { \_ -> TokenInt 0 }
    "-"$positive$digit*   { \s -> TokenInt (read s) }
    $positive$digit*      { \s -> TokenInt (read s) }

    -- identificadores de variables
    $letter$id_rest* { \s -> TokenVar s }

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
    | TokenCond
    | TokenElse
    | TokenLet
    | TokenLetStar
    | TokenLetRec
    | TokenIf
    | TokenIf0
    | TokenLambda
    | TokenAdd1
    | TokenSub1
    | TokenSqrt
    | TokenExpt
    | TokenHead
    | TokenTail
    | TokenNot
    | TokenFst
    | TokenSnd
    | TokenPair
    deriving (Show, Eq)

-- para ejecutar el lexer
lexer :: String -> [Token]
lexer = alexScanTokens
}
