-- #################################################
-- ## Archivo: Parser.y (Versión de Prueba Aislada)
-- #################################################
{
-- Cabecera de Haskell
module Parser (parser, ExprS(..)) where -- Exportamos ExprS desde aquí

import Lexer (Token(..))
-- ¡NO importamos AbsSyn.hs!
}

-- Directivas de Happy
%name parser ExprS
%tokentype { Token }
%error { parseError }

-- Mapeo de Tokens
%token
    '('         { TokenLPar }
    ')'         { TokenRPar }
    '['         { TokenLCor }
    ']'         { TokenRCor }
    ','         { TokenComa }
    int         { TokenInt $$ }
    bool        { TokenBool $$ }
    var         { TokenVar $$ }
    'let'       { TokenReserv "let" }
    'let*'      { TokenReserv "let*" }
    'letrec'    { TokenReserv "letrec" }
    'if'        { TokenReserv "if" }
    'if0'       { TokenReserv "if0" }
    'lambda'    { TokenReserv "lambda" }
    'cond'      { TokenReserv "cond" }
    'else'      { TokenReserv "else" }
    'head'      { TokenReserv "head" }
    'tail'      { TokenReserv "tail" }
    'pair'      { TokenReserv "pair" }
    'fst'       { TokenReserv "fst" }
    'snd'       { TokenReserv "snd" }
    'add1'      { TokenReserv "add1" }
    'sub1'      { TokenReserv "sub1" }
    'sqrt'      { TokenReserv "sqrt" }
    'expt'      { TokenReserv "expt" }
    'not'       { TokenReserv "not" }
    '+'         { TokenOp "+" }
    '-'         { TokenOp "-" }
    '*'         { TokenOp "*" }
    '/'         { TokenOp "/" }
    '='         { TokenOp "=" }
    '<'         { TokenOp "<" }
    '>'         { TokenOp ">" }
    '<='        { TokenOp "<=" }
    '>='        { TokenOp ">=" }
    '!='        { TokenOp "!=" }

%%
-- Reglas de Gramática
ExprS :
      int                       { NumS $1 }
    | bool                      { BoolS $1 }
    | var                       { IdS $1 }
    | '[' Exprs ']'             { ListS $2 }
    | '(' 'let' '(' Bindings ')' ExprS ')'      { LetS $4 $6 }
    | '(' 'let*' '(' Bindings ')' ExprS ')'     { LetStarS $4 $6 }
    | '(' 'letrec' '(' Bindings ')' ExprS ')'   { LetRecS $4 $6 }
    | '(' 'cond' Clauses OptElse ')'            { CondS $3 $4 } -- Produce [(ExprS, ExprS)]
    | '(' 'if' ExprS ExprS ExprS ')'            { IfS $3 $4 $5 }
    | '(' 'if0' ExprS ExprS ExprS ')'           { If0S $3 $4 $5 }
    | '(' 'lambda' '(' Vars ')' ExprS ')'       { FunS $4 $6 }
    | '(' 'pair' ExprS ExprS ')'                { PairS $3 $4 }
    | '(' 'fst' ExprS ')'                       { FstS $3 }
    | '(' 'snd' ExprS ')'                       { SndS $3 }
    | '(' 'head' ExprS ')'                      { HeadS $3 }
    | '(' 'tail' ExprS ')'                      { TailS $3 }
    | '(' 'not' ExprS ')'                       { NotS $3 }
    | '(' 'add1' ExprS ')'                      { Add1S $3 }
    | '(' 'sub1' ExprS ')'                      { Sub1S $3 }
    | '(' 'sqrt' ExprS ')'                      { SqrtS $3 }
    | '(' 'expt' ExprS ExprS ')'                { ExptS $3 $4 }
    | '(' '+' ExprS ExprSMore ')'               { AddS ($3 : $4) }
    | '(' '-' ExprS ExprSMore ')'               { SubS ($3 : $4) }
    | '(' '*' ExprS ExprSMore ')'               { MulS ($3 : $4) }
    | '(' '/' ExprS ExprSMore ')'               { DivS ($3 : $4) }
    | '(' '=' ExprS ExprSMore ')'               { EqS ($3 : $4) }
    | '(' '!=' ExprS ExprSMore ')'              { NeqS ($3 : $4) }
    | '(' '<' ExprS ExprSMore ')'               { LtS ($3 : $4) }
    | '(' '>' ExprS ExprSMore ')'               { GtS ($3 : $4) }
    | '(' '<=' ExprS ExprSMore ')'              { LteS ($3 : $4) }
    | '(' '>=' ExprS ExprSMore ')'              { GteS ($3 : $4) }
    | '(' ExprS ExprSMore ')'                   { AppS $2 $3 }
Bindings :
      Binding                   { [$1] }
    | Binding Bindings          { $1 : $2 }
Binding :
    '(' var ExprS ')'           { ($2, $3) }
Clauses :
      { [] }
    | Clause Clauses            { $1 : $2 }
Clause :
    '[' ExprS ExprS ']'         { ($2, $3) } -- Produce (ExprS, ExprS)
OptElse :
      { Nothing }
    | '[' 'else' ExprS ']'      { Just $3 }
Vars :
      { [] }
    | var Vars                  { $1 : $2 }
Exprs :
      { [] }
    | ExprS ExprsRest           { $1 : $2 }
ExprsRest :
      { [] }
    | ',' ExprS ExprsRest       { $2 : $3 }
ExprSMore :
      ExprS                     { [$1] }
    | ExprS ExprSMore           { $1 : $2 }
{
-- #################################################
-- ##             PIE DE PÁGINA (Haskell)         ##
-- #################################################

-- La función de error
parseError :: [Token] -> a
parseError tokens = error ("Error de sintaxis cerca de: " ++ show (take 10 tokens))

-- #################################################
-- ##       DEFINICIÓN DE ExprS (¡AQUÍ MISMO!)    ##
-- #################################################
data ExprS
  = IdS String
  | NumS Int
  | BoolS Bool
  | AddS [ExprS]
  | SubS [ExprS]
  | MulS [ExprS]
  | DivS [ExprS]
  | Add1S ExprS
  | Sub1S ExprS
  | SqrtS ExprS
  | ExptS ExprS ExprS
  | NotS ExprS
  | EqS [ExprS]
  | NeqS [ExprS]
  | LtS [ExprS]
  | GtS [ExprS]
  | LteS [ExprS]
  | GteS [ExprS]
  | PairS ExprS ExprS
  | FstS ExprS
  | SndS ExprS
  | LetS [(String, ExprS)] ExprS
  | LetStarS [(String, ExprS)] ExprS
  | LetRecS [(String, ExprS)] ExprS
  | If0S ExprS ExprS ExprS
  | IfS ExprS ExprS ExprS
  | CondS [(ExprS, ExprS)] (Maybe ExprS) -- <<<<<------ ¡AQUÍ ESTÁ LA CORRECCIÓN!
  | ListS [ExprS]
  | HeadS ExprS
  | TailS ExprS
  | FunS [String] ExprS
  | AppS ExprS [ExprS]
  deriving (Show, Eq)
}