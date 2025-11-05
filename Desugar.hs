-- #################################################
-- ## Archivo: Desugar.hs (Corregido y Refactorizado) ##
-- #################################################
module Desugar (desugar, ExprC (..), Value (..), Env, Id) where

-- Importamos la definición de ExprS (ASA de Superficie) desde el Parser
import Parser (ExprS (..))

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
-- ¡OBSERVA QUE LetC HA SIDO ELIMINADO!
data ExprC
  = IdC Id -- Identificadores
  | NumC Int -- Literales numéricas
  | BoolC Bool -- Literales booleanas
  | LamC Id ExprC -- Funciones (un solo argumento - currificadas)
  | AppC ExprC ExprC -- Aplicación de funciones (un solo argumento)
  | RestoreC ExprC Env -- evalúa un cuerpo con el env del closure y restaura el env del llamador
  | IfC ExprC ExprC ExprC -- Único condicional
  -- Operadores/Predicados Nucleo (Binarios o Unarios)
  | AddC ExprC ExprC
  | SubC ExprC ExprC
  | MulC ExprC ExprC
  | DivC ExprC ExprC
  | EqC ExprC ExprC
  | NeqC ExprC ExprC -- !=
  | LtC ExprC ExprC -- <
  | GtC ExprC ExprC -- >
  | LteC ExprC ExprC -- <=
  | GteC ExprC ExprC -- >=
  | NotC ExprC -- not
  | Add1C ExprC
  | Sub1C ExprC
  | SqrtC ExprC -- Raiz
  | ExptC ExprC ExprC -- Exponencial
  -- Primitivas de Pares y Listas
  | PairC ExprC ExprC -- Constructor de pares (expresiones como hijos)
  | FstC ExprC -- Proyección primer elemento
  | SndC ExprC -- Proyección segundo elemento
  | NilC -- Representación de lista vacía en el núcleo
  -- Constructor *interno* para el intérprete smallStep:
  | ValC Value -- Encapsula un Valor dentro de una Expresión
  deriving (Show, Eq)

-- #################################################
-- ##       FUNCIONES AUXILIARES                  ##
-- #################################################

-- Transforma una lista de parámetros en lambdas anidadas (currificación)
-- Ej: (lambda (x y) body) -> (lambda (x) (lambda (y) body))
curryLambda :: [Id] -> ExprC -> ExprC
curryLambda [] bodyC = bodyC
curryLambda (p : ps) bodyC = LamC p (curryLambda ps bodyC)

-- Transforma una aplicación múltiple en aplicaciones anidadas (asociatividad izquierda)
-- Ej: (f a b c) -> (((f a) b) c)
applyCurried :: ExprC -> [ExprC] -> ExprC
applyCurried funC [] = funC -- Aplicación de función sin argumentos (ej. 'f')
applyCurried funC (a : args) = foldl AppC (AppC funC a) args

-- Función auxiliar para desazucarizar operadores variádicos binarios
-- (Mapea, desazucariza y aplica el operador de izquierda a derecha)
desugarVariadicOp :: (ExprC -> ExprC -> ExprC) -> [ExprS] -> ExprC
desugarVariadicOp op listExprS =
  let listExprC = map desugar listExprS
   in foldl1 op listExprC

-- Función auxiliar para desazucarizar operadores variádicos de comparaciones
-- (Genera una cadena de comparaciones anidadas)
desugarChain :: (ExprC -> ExprC -> ExprC) -> [ExprS] -> ExprC
desugarChain comp [] = BoolC True
desugarChain comp [_] = BoolC True
desugarChain comp (e1: e2 : esRest) = IfC (comp (desugar e1) (desugar e2)) (desugarChain comp (e2 : esRest)) (BoolC False)

-- combinador Z (punto fijo para evaluación por valor)
fixCombinator :: ExprC
fixCombinator =
  LamC "g" $
    AppC
      (LamC "x" (AppC (IdC "g") (LamC "v" (AppC (AppC (IdC "x") (IdC "x")) (IdC "v")))))
      (LamC "x" (AppC (IdC "g") (LamC "v" (AppC (AppC (IdC "x") (IdC "x")) (IdC "v")))))


-- #################################################
-- ##       FUNCIÓN PRINCIPAL DE DESAZUCARIZACIÓN ##
-- #################################################
desugar :: ExprS -> ExprC
desugar (IdS s) = IdC s
desugar (NumS n) = NumC n
desugar (BoolS b) = BoolC b

-- Operadores Aritméticos y Relacionales (Variádicos)
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
desugar (PairS e1 e2) = PairC (desugar e1) (desugar e2)
desugar (FstS e) = FstC (desugar e)
desugar (SndS e) = SndC (desugar e)
desugar (ListS []) = NilC
desugar (ListS es) = foldr PairC NilC (map desugar es) -- encadenamiento de pares con NilC 
desugar (HeadS e) = FstC (desugar e)
desugar (TailS e) = SndC (desugar e)

-- Condicionales
desugar (IfS c t e) = IfC (desugar c) (desugar t) (desugar e)
desugar (If0S c t e) = IfC (EqC (desugar c) (NumC 0)) (desugar t) (desugar e)
desugar (CondS clauses maybeElse) = desugarCond clauses (desugarElse maybeElse)
  where
    desugarElse Nothing = error "Error: 'cond' sin cláusula 'else'"
    desugarElse (Just e) = desugar e
    desugarCond [] elseExpr = elseExpr
    desugarCond ((guardExpr, bodyExpr) : rest) elseExpr =
      IfC (desugar guardExpr) (desugar bodyExpr) (desugarCond rest elseExpr)

-- --- INICIO DE CAMBIOS SOLICITADOS ---

-- REGLA PARA LetS (Paralelo)
-- (let ((x v1) (y v2)) body) -> ((lambda (x y) body) v1 v2)
desugar (LetS bindings body) =
  let (vars, valsS) = unzip bindings
      valsC = map desugar valsS
      bodyC = desugar body
  -- Usa los helpers para currificación y aplicación múltiple
  in applyCurried (curryLambda vars bodyC) valsC

-- REGLA PARA LetStarS (Secuencial)
-- (let* ((x v1) (y v2)) body) -> ((lambda (x) ((lambda (y) body) v2)) v1)
desugar (LetStarS bindings body) = desugarLetStar (map desugarBinding bindings) (desugar body)
  where
    desugarBinding (var, valS) = (var, desugar valS)
    -- Caso base: no más bindings, solo queda el cuerpo
    desugarLetStar [] bodyC = bodyC
    -- Caso recursivo: (let* ((var val) ...rest) body)
    -- se convierte en: ((lambda (var) (let* (...rest) body)) val)
    desugarLetStar ((var, valC) : rest) bodyC =
      AppC (LamC var (desugarLetStar rest bodyC)) valC

-- REGLA PARA LetRecS (Recursivo)
-- (letrec ((f (lambda (p) ...))) body) ->
--   ((lambda (f) body) (Z (lambda (f) (lambda (p) ...))))
-- REGLA PARA LetRecS (Corregida)
desugar (LetRecS bindings body) = desugarLetRec bindings (desugar body)
  where
    desugarLetRec [] bodyC = bodyC
    desugarLetRec ((f, FunS params fbody) : rest) bodyC =
        -- g = (lambda (f) (lambda (params...) fbody))
        let g   = LamC f (desugar (FunS params fbody))
            -- z_g = (Z g)
            z_g = AppC fixCombinator g
        -- ((lambda (f) (desugarLetRec rest bodyC)) z_g)
        in AppC (LamC f (desugarLetRec rest bodyC)) z_g
        
    desugarLetRec ((f, val) : _rest) _bodyC =
      error ("letrec: RHS no es una función para '" ++ f ++ "' — letrec solo admite bindings de funciones en esta implementación")

-- REGLA PARA FunS (AHORA USA EL HELPER)
desugar (FunS params body) = curryLambda params (desugar body)

-- REGLA PARA AppS (AHORA USA EL HELPER)
desugar (AppS funExpr argExprs) = applyCurried (desugar funExpr) (map desugar argExprs)

-- --- FIN DE CAMBIOS SOLICITADOS ---