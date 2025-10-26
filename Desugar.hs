-- #################################################
-- ## Archivo: Desugar.hs (Modificado)            ##
-- #################################################
module Desugar (desugar, ExprC(..), Value(..), Env, Id) where

-- Importamos la definición de ExprS (ASA de Superficie) desde el Parser
import Parser (ExprS(..))

-- Definimos un alias para los identificadores (Strings)
type Id = String

-- #################################################
-- ##          DEFINICIÓN DE VALORES Y ENTORNO    ##
-- #################################################
-- Define los posibles resultados *finales* de la evaluación (Valores).
data Value
  = NumV Int
  | BoolV Bool
  | ClosureV Id ExprC Env -- Parámetro, Cuerpo, Entorno capturado (Alcance Estático)
  | PairV Value Value
  | NilV
  deriving (Eq, Show)

-- Entorno de ejecución: mapea Identificadores (Strings) a Valores
type Env = [(Id, Value)]

-- #################################################
-- ##       DEFINICIÓN DE ExprC (ASA Núcleo)      ##
-- #################################################
-- Contiene las construcciones mínimas + ValC para el intérprete.
data ExprC
  = IdC Id                  -- Identificadores
  | NumC Int                -- Literales numéricas
  | BoolC Bool              -- Literales booleanas
  | LamC Id ExprC           -- Funciones (un solo argumento - currificadas)
  | AppC ExprC ExprC        -- Aplicación de funciones (un solo argumento)
  | LetC Id ExprC ExprC     -- Único constructor de asignación
  | IfC ExprC ExprC ExprC   -- Único condicional
  -- Operadores/Predicados Nucleo (Binarios o Unarios)
  | AddC ExprC ExprC
  | SubC ExprC ExprC
  | MulC ExprC ExprC
  | DivC ExprC ExprC
  | EqC ExprC ExprC
  | NeqC ExprC ExprC
  | LtC ExprC ExprC
  | GtC ExprC ExprC
  | LteC ExprC ExprC
  | GteC ExprC ExprC
  | NotC ExprC
  | Add1C ExprC
  | Sub1C ExprC
  | SqrtC ExprC
  | ExptC ExprC ExprC
  -- Primitivas de Pares y Listas
  | PairC ExprC ExprC       -- Constructor de pares (expresiones como hijos)
  | FstC ExprC              -- Proyección primer elemento
  | SndC ExprC              -- Proyección segundo elemento
  | NilC                    -- Representación de lista vacía en el núcleo
  -- Constructor *interno* para el intérprete smallStep:
  | ValC Value              -- Encapsula un Valor dentro de una Expresión
  deriving (Show, Eq)

-- #################################################
-- ##       FUNCIÓN PRINCIPAL DE DESAZUCARIZACIÓN ##
-- #################################################
desugar :: ExprS -> ExprC
desugar (IdS s)     = IdC s
desugar (NumS n)    = NumC n
desugar (BoolS b)   = BoolC b
desugar (AddS es) = foldr1 AddC (map desugar es)
desugar (SubS es) = foldr1 SubC (map desugar es)
desugar (MulS es) = foldr1 MulC (map desugar es)
desugar (DivS es) = foldr1 DivC (map desugar es)
desugar (EqS es)  = desugarVariadicPredicate EqC (map desugar es)
desugar (NeqS es) = desugarVariadicPredicate NeqC (map desugar es)
desugar (LtS es)  = desugarVariadicPredicate LtC (map desugar es)
desugar (GtS es)  = desugarVariadicPredicate GtC (map desugar es)
desugar (LteS es) = desugarVariadicPredicate LteC (map desugar es)
desugar (GteS es) = desugarVariadicPredicate GteC (map desugar es)
desugar (NotS e)    = NotC (desugar e)
desugar (Add1S e)   = Add1C (desugar e)
desugar (Sub1S e)   = Sub1C (desugar e)
desugar (SqrtS e)   = SqrtC (desugar e)
desugar (ExptS e1 e2) = ExptC (desugar e1) (desugar e2)
desugar (PairS e1 e2) = PairC (desugar e1) (desugar e2)
desugar (FstS e)    = FstC (desugar e)
desugar (SndS e)    = SndC (desugar e)
desugar (ListS [])  = NilC
desugar (ListS es)  = foldr PairC NilC (map desugar es)
desugar (HeadS e)   = FstC (desugar e)
desugar (TailS e)   = SndC (desugar e)
desugar (IfS c t e) = IfC (desugar c) (desugar t) (desugar e)
desugar (If0S c t e)= IfC (EqC (desugar c) (NumC 0)) (desugar t) (desugar e)
desugar (CondS clauses maybeElse) = desugarCond clauses (desugarElse maybeElse)
  where
    desugarElse Nothing  = error "Error: 'cond' sin cláusula 'else'"
    desugarElse (Just e) = desugar e
    desugarCond [] elseExpr = elseExpr
    desugarCond ((guardExpr, bodyExpr):rest) elseExpr =
      IfC (desugar guardExpr) (desugar bodyExpr) (desugarCond rest elseExpr)

desugar (LetS bindings body) = desugarLet bindings (desugar body)
  where
    desugarLet [] bodyC = bodyC
    desugarLet ((var, val):rest) bodyC = LetC var (desugar val) (desugarLet rest bodyC)

desugar (LetStarS bindings body) = desugarLetStar bindings (desugar body)
  where
    desugarLetStar [] bodyC = bodyC
    desugarLetStar ((var, val):rest) bodyC = LetC var (desugar val) (desugarLetStar rest bodyC)

desugar (LetRecS bindings body) = desugarLetRec bindings (desugar body)
  where
    yCombinator = IdC "Y"
    desugarLetRec [] bodyC = bodyC
    desugarLetRec ((f, FunS params fbody):rest) bodyC =
       let funExpr = desugar (FunS params fbody)
           yAbstract = LamC f funExpr
       in LetC f (AppC yCombinator yAbstract) (desugarLetRec rest bodyC)
    desugarLetRec ((f, val):rest) bodyC =
       let valExpr = desugar val
           yAbstract = LamC f valExpr
       in LetC f (AppC yCombinator yAbstract) (desugarLetRec rest bodyC)

desugar (FunS params body) = curryLambda params (desugar body)
  where
    curryLambda [] bodyC = bodyC
    curryLambda (p:ps) bodyC = LamC p (curryLambda ps bodyC)

desugar (AppS funExpr argExprs) = applyCurried (desugar funExpr) (map desugar argExprs)
  where
    applyCurried funC [] = funC
    applyCurried funC (a:args) = foldl AppC (AppC funC a) args

-- No exportamos ValC desde desugar, ya que es interno al intérprete
-- desugar (ValC _) = error "ValC no debería aparecer antes de la interpretación"

-- #################################################
-- ##       FUNCIONES AUXILIARES                  ##
-- #################################################
desugarVariadicPredicate :: (ExprC -> ExprC -> ExprC) -> [ExprC] -> ExprC
desugarVariadicPredicate _ [] = BoolC True -- O error
desugarVariadicPredicate _ [_] = BoolC True -- O error
desugarVariadicPredicate opConstructor (e1:e2:rest) = go (opConstructor e1 e2) e2 rest
  where
    go acc _ [] = acc
    go acc prev (e:es) =
      let currentPred = opConstructor prev e
          newAcc = IfC acc currentPred (BoolC False) -- Simula AND
      in go newAcc e es