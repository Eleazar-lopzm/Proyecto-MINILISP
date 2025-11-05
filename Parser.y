-- #################################################
-- ## Archivo: Parser.y                           ##
-- #################################################
{
module Parser (parser, ExprS(..)) where

import Lexer (Token(..))
}

-- Directivas de Happy
%name parser  ExprS 
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
    'let'       { TokenLet }
    'let*'      { TokenLetStar }
    'letrec'    { TokenLetRec }
    'if'        { TokenIf }
    'if0'       { TokenIf0 }
    'lambda'    { TokenLambda }
    'cond'      { TokenCond }
    'else'      { TokenElse }
    'head'      { TokenHead }
    'tail'      { TokenTail }
    'pair'      { TokenPair }
    'fst'       { TokenFst }
    'snd'       { TokenSnd }
    'add1'      { TokenAdd1 }
    'sub1'      { TokenSub1 }
    'sqrt'      { TokenSqrt }
    'expt'      { TokenExpt }
    'not'       { TokenNot }
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
    | '(' 'cond' CondClauses ')'    {
        -- $3 :: [Either (ExprS, ExprS) ExprS]
        let (clauses, mElse) = foldr
              (\e (cs, me) -> case e of
                                Left pair -> (pair:cs, me)
                                Right v   -> (cs, Just v))
              ([], Nothing)
              $3
        in CondS clauses mElse
    }
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

-- Reglas desambiguadas para 'cond'
CondClauses :
      CondClause                  { [$1] }
    | CondClause CondClauses       { $1 : $2 }

CondClause :
      '[' ExprS ExprS ']'        { Left ($2, $3) }
    | '[' 'else' ExprS ']'       { Right $3 }


Vars :
      { [] }
    | var Vars                  { $1 : $2 }

Exprs :
      { [] }
    | ExprS ExprsRest           { $1 : $2 }

-- Cambia ExprsRest para permitir espacios
ExprsRest :
      { [] }
    | ExprS ExprsRest           { $1 : $2 }  -- Permite espacios
    | ',' ExprS ExprsRest       { $2 : $3 }  -- También permite comas

ExprSMore :
      ExprS                     { [$1] }
    | ExprS ExprSMore           { $1 : $2 }

{

-- La función de error
parseError :: [Token] -> a
parseError tokens = error ("Error de sintaxis cerca de: " ++ show (take 10 tokens))


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
  | CondS [(ExprS, ExprS)] (Maybe ExprS) 
  | ListS [ExprS]
  | HeadS ExprS
  | TailS ExprS
  | FunS [String] ExprS
  | AppS ExprS [ExprS]
  deriving (Show, Eq)
}
