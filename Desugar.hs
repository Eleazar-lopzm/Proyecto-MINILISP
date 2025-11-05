-- #################################################
-- ##             Archivo: Desugar.hs             ##
-- #################################################
-- Este módulo implementa la fase de desazucaración del lenguaje.
-- Convierte expresiones de la superficie (ExprS) a expresiones del core (ExprC)
-- descartando el azúcar sintáctica como let*, cond, listas, entre otros.

module Desugar (desugar, ExprC (..), Value (..), Env, Id) where


import Parser (ExprS (..))

-- Identificadores se representan como strings
type Id = String

-- #################################################
-- ##        DEFINICIÓN DE VALORES Y ENTORNO      ##
-- #################################################
data Value
  = NumV Int
  | BoolV Bool
  | ClosureV Id ExprC Env 
  | PairV Value Value
  | NilV
  deriving (Eq, Show)

-- Ambientes de evaluación son listas de pares (nombre,valor)
type Env = [(Id, Value)]

-- #################################################
-- ##       DEFINICIÓN DE ExprC (ASA Núcleo)      ##
-- #################################################
data ExprC
  = IdC Id -- Identificadores
  | NumC Int -- Literales numéricas
  | BoolC Bool -- Literales booleanas
  | LamC Id ExprC -- Funciones (un solo argumento - currificadas)
  | AppC ExprC ExprC -- Aplicación de funciones (un solo argumento)
  | RestoreC ExprC Env -- evalúa un cuerpo con el env del closure y restaura el env del llamador
  | IfC ExprC ExprC ExprC -- Único condicional
  -- Operadores/Predicados Nucleo (Binarios o Unarios)
  | AddC ExprC ExprC -- +
  | SubC ExprC ExprC -- -
  | MulC ExprC ExprC -- *
  | DivC ExprC ExprC -- /
  | EqC ExprC ExprC -- =
  | NeqC ExprC ExprC -- !=
  | LtC ExprC ExprC -- <
  | GtC ExprC ExprC -- >
  | LteC ExprC ExprC -- <=
  | GteC ExprC ExprC -- >=
  | NotC ExprC -- not
  | Add1C ExprC -- +1
  | Sub1C ExprC -- -1
  | SqrtC ExprC -- Raiz
  | ExptC ExprC ExprC -- Exponencial
  -- Primitivas de Pares y Listas
  | PairC ExprC ExprC -- Constructor de pares 
  | FstC ExprC -- Proyección primer elemento
  | SndC ExprC -- Proyección segundo elemento
  | NilC -- Representación de lista vacía en el núcleo
  | ValC Value 
  deriving (Show, Eq)

-- #################################################
-- ##            FUNCIONES AUXILIARES             ##
-- #################################################

-- Transforma una lista de parámetros en lambdas anidadas (currificación)
-- Ej: (lambda (x y) body) -> LamC "x" (LamC "y" bodyC)
-- Desazucara una función de varios argumentos en funciones unarias anidadas
-- preservando el orden de los argumentos
curryLambda :: [Id] -> ExprC -> ExprC
curryLambda [] bodyC = bodyC
curryLambda (p : ps) bodyC = LamC p (curryLambda ps bodyC)

-- Transforma una aplicación múltiple en aplicaciones anidadas (asociatividad izquierda)
-- Ej: (f a b c) -> (((f a) b) c)
-- Si la lista de argumentos es vacía, devuelve la función sin aplicar
applyCurried :: ExprC -> [ExprC] -> ExprC
applyCurried funC [] = funC 
applyCurried funC (a : args) = foldl AppC (AppC funC a) args

-- Función auxiliar para desazucarizar operadores variádicos binarios
-- Desazucara cada operando y los agrupa en operadores binarios con asociatividad izquierda
-- Ej: (+ a b c) -> + (+ a b) c
-- NOTA: esta función requiere al menos un operando
desugarVariadicOp :: (ExprC -> ExprC -> ExprC) -> [ExprS] -> ExprC
desugarVariadicOp op listExprS =
  let listExprC = map desugar listExprS
   in foldl1 op listExprC

-- Función auxiliar para desazucarizar operadores variádicos de comparaciones
-- Ej: (< a b c)  -> (if (< a b) (if (< b c) #t #f) #f)
-- Nota: si se tienen menos de dos operandos, devuelve True por defecto
desugarChain :: (ExprC -> ExprC -> ExprC) -> [ExprS] -> ExprC
desugarChain comp [] = BoolC True
desugarChain comp [_] = BoolC True
desugarChain comp (e1: e2 : esRest) = IfC (comp (desugar e1) (desugar e2)) (desugarChain comp (e2 : esRest)) (BoolC False)

-- combinador Z (combinador de punto fijo para evaluación por valor)
-- utilizado para desazucarar letrec
fixCombinator :: ExprC
fixCombinator =
  LamC "g" $
    AppC
      (LamC "x" (AppC (IdC "g") (LamC "v" (AppC (AppC (IdC "x") (IdC "x")) (IdC "v")))))
      (LamC "x" (AppC (IdC "g") (LamC "v" (AppC (AppC (IdC "x") (IdC "x")) (IdC "v")))))


-- #################################################
-- ##       FUNCIÓN PRINCIPAL DE DESAZUCARIZACIÓN ##
-- #################################################

-- Función que mapea ExprS (superficie) a ExprC (core) 
desugar :: ExprS -> ExprC

-- Identificadores y valores
-- nil es una constante tratada como la cadena vacía
desugar (IdS "nil") = NilC
desugar (IdS s) = IdC s
desugar (NumS n) = NumC n
desugar (BoolS b) = BoolC b

-- Operadores Aritméticos y Relacionales (Variádicos)
-- Operadores aritméticos binarios se anidan con asociatividad izquierda mediante desugarVariadicOp
-- Operadores relacionales se transforman en ifs anidados con desugarChain
desugar (AddS es) = desugarVariadicOp AddC es
desugar (SubS es) = desugarVariadicOp SubC es
desugar (MulS es) = desugarVariadicOp MulC es
desugar (DivS es) = desugarVariadicOp DivC es
desugar (EqS es) = desugarChain EqC es
desugar (NeqS es) = desugarChain NeqC es
desugar (LtS es) = desugarChain LtC es
desugar (GtS es) = desugarChain GtC es
desugar (LteS es) = desugarChain LteC es
desugar (GteS es) = desugarChain GteC es

-- Operadores Unarios y Primitivas
desugar (NotS e) = NotC (desugar e)
desugar (Add1S e) = Add1C (desugar e)
desugar (Sub1S e) = Sub1C (desugar e)
desugar (SqrtS e) = SqrtC (desugar e)
desugar (ExptS e1 e2) = ExptC (desugar e1) (desugar e2)

-- Pares y Listas
-- Listas se desazucaran en pares anidados que asocian a la derecha
-- HeadS y TailC se mapean a FstC y SndC gracias a el mapeo de listas a pares
desugar (PairS e1 e2) = PairC (desugar e1) (desugar e2)
desugar (FstS e) = FstC (desugar e)
desugar (SndS e) = SndC (desugar e)
desugar (ListS []) = NilC
desugar (ListS es) = foldr PairC NilC (map desugar es) -- encadenamiento de pares con NilC 
desugar (HeadS e) = FstC (desugar e)
desugar (TailS e) = SndC (desugar e)

-- Condicionales
-- If0S es azúcar sintáctica: (if (= c 0) t e) utilizando operador relacional EqC
-- CondS con una lista de pares (guard,body) se desazucara en IfCs anidados
-- y la cláusula final else debe estar presente, de otro modo se arroja un error 
desugar (IfS c t e) = IfC (desugar c) (desugar t) (desugar e)
desugar (If0S c t e) = IfC (EqC (desugar c) (NumC 0)) (desugar t) (desugar e)
desugar (CondS clauses maybeElse) = desugarCond clauses (desugarElse maybeElse)
  where
    desugarElse Nothing = error "Error: 'cond' sin cláusula 'else'"
    desugarElse (Just e) = desugar e
    desugarCond [] elseExpr = elseExpr
    desugarCond ((guardExpr, bodyExpr) : rest) elseExpr =
      IfC (desugar guardExpr) (desugar bodyExpr) (desugarCond rest elseExpr)

-- REGLA PARA LetS (Simultáneo)
-- Ej: (let ((x v1) (y v2)) body) -> ((lambda (x y) body) v1 v2)
-- Se extraen las variables vars y se obtienen los valores desazucarados valsC,
-- se desazucara el cuerpo y se currifica la función con curryLambda, y luego
-- esta se le aplica a valsC con applyCurried, produciendo aplicaciones anidadas
desugar (LetS bindings body) =
  let (vars, valsS) = unzip bindings
      valsC = map desugar valsS
      bodyC = desugar body
 
  in applyCurried (curryLambda vars bodyC) valsC

-- REGLA PARA LetStarS (Secuencial)
-- Ej: (let* ((x v1) (y v2)) body) -> ((lambda (x) ((lambda (y) body) v2)) v1)
-- Primero se desazucara cada binding y el cuerpo, y se anidan aplicaciones de
-- un solo argumento
desugar (LetStarS bindings body) = desugarLetStar (map desugarBinding bindings) (desugar body)
  where
    desugarBinding (var, valS) = (var, desugar valS)
    desugarLetStar [] bodyC = bodyC
    desugarLetStar ((var, valC) : rest) bodyC =
      AppC (LamC var (desugarLetStar rest bodyC)) valC

-- REGLA PARA LetRecS (Recursivo)
-- Ej: (letrec ((f (lambda (p) ...))) body) ->
--      ((lambda (f) body) (Z (lambda (f) (lambda (p) ...))))
-- Para cada binding (f, FunS params fbody) construímos λf.(λparams.fbodyC)
-- y (Z g), y luego anidamos las llamadas con AppC (LamC f restBodyC) z_g
-- Si el valor en un binding no es una función, se genera un error
desugar (LetRecS bindings body) = desugarLetRec bindings (desugar body)
  where
    desugarLetRec [] bodyC = bodyC
    desugarLetRec ((f, FunS params fbody) : rest) bodyC =
        let g   = LamC f (desugar (FunS params fbody))
            z_g = AppC fixCombinator g
        in AppC (LamC f (desugarLetRec rest bodyC)) z_g
        
    desugarLetRec ((f, val) : _rest) _bodyC =
      error ("letrec: No es una función para '" ++ f ++ "' — letrec solo admite bindings de funciones en esta implementación")

-- REGLA PARA FunS (funciones con múltiples parámetros)
-- Ej: (lambda (x y z) body) -> (lambda (x) (lambda (y) (lambda (z) body))) 
-- Funciones con varios parámetros se currifican (se desazucaran usando curryLambda)
desugar (FunS params body) = curryLambda params (desugar body)

-- REGLA PARA AppS (aplicaciones con múltiples argumentos)
-- Ej: (f a b c) -> (((f a) b) c)
-- Aplicaciones con varios argumentos se desazucaran usando applyCurried
-- La función es aplicada a los argumentos uno por uno con asociatividad izquierda
desugar (AppS funExpr argExprs) = applyCurried (desugar funExpr) (map desugar argExprs)

