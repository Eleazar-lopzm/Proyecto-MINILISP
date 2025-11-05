-- #################################################
-- ##              Archivo: Interp.hs             ##
-- #################################################
-- Este módulo implementa el intérprete small-step del lenguaje.
-- Define las reglas de evaluación de ExprC bajo un ambiente Env.

module Interp (interp, Value(..), Env) where

import Desugar (ExprC(..), Value(..), Env, Id)
import Prelude hiding (lookup)
import Debug.Trace (trace) 

-- #################################################
-- ##       FUNCIONES isValue, lookup, etc.       ##
-- #################################################

-- Función que determina si una expresión en el core es un valor
isValue :: ExprC -> Bool
isValue (NumC _)  = True
isValue (BoolC _) = True
isValue NilC      = True
isValue (ValC v)  = isValueVal v
isValue _         = False

-- Función que determina si un Value está bien formado: los pares
-- se validan recursivamente. Otros valores son válidos por construcción
isValueVal :: Value -> Bool
isValueVal (PairV v1 v2) = isValueVal v1 && isValueVal v2
isValueVal _             = True

-- Función que busca un identificador en el ambiente
-- Arroja un error si el identificador no existe en el ambiente (variable libre)
lookup :: Id -> Env -> Value
lookup var [] = error ("Variable libre: " ++ var)
lookup var ((name, val):rest)
  | var == name = val
  | otherwise   = lookup var rest

-- Función que revisa si una expresión en el core es una abstracción lambda
-- (no considera closures)
isLam :: ExprC -> Bool
isLam (LamC _ _) = True
isLam _          = False

-- Función que convierte un Value a su expresión correspondiente en el core
-- Closures y pares se convierten en ValC v, indicando que son valores finales
valueToExpr :: Value -> ExprC
valueToExpr (NumV n) = NumC n
valueToExpr (BoolV b) = BoolC b
valueToExpr NilV = NilC
valueToExpr v@(ClosureV _ _ _) = ValC v
valueToExpr v@(PairV _ _) = ValC v

-- Función que convierte una expresión en el core, que representa un valor,
-- en el Value que le corresponde. Arroja un error si la expresión no es un valor
exprToValue :: ExprC -> Value
exprToValue (NumC n) = NumV n
exprToValue (BoolC b) = BoolV b
exprToValue NilC = NilV
exprToValue (ValC v) = v
exprToValue e = error ("Intentando convertir ExprC no-valor a Value: " ++ show e)

-- Función que determina si una expresión es un closure ya evaluado
isClosure :: ExprC -> Bool
isClosure (ValC (ClosureV _ _ _)) = True
isClosure _                       = False

-- #################################################
-- ##       FUNCIÓN PRINCIPAL: smallStep          ##
-- #################################################

-- Función que hace un paso de la reducción de una expresión 
-- en el core bajo cierto ambiente. La evaluación es call-by-value
-- Cada paso devuelve la nueva expresión con el ambiente actualizado
smallStep :: ExprC -> Env -> (ExprC, Env)

-- REGLAS PARA Valores y Variables
-- Casos base: si la expresión ya es un valor, es irreducible
-- Nota: IdC i no es un valor, se reduce a (ValC (lookup i env)),
-- es decir, el identificador se sustituye por su valor en el ambiente
smallStep e@(NumC _) env  = (e, env)
smallStep e@(BoolC _) env = (e, env)
smallStep e@NilC env      = (e, env)
smallStep e@(ValC v) env | isValueVal v = (e, env)
smallStep (IdC i) env = (ValC (lookup i env), env)

-- REGLA PARA Funciones 
-- Al evaluar una abstracción lambda, se captura el ambiente actual con un closure
-- (alcance estático). El resultado es un valor.
smallStep (LamC param body) env = (ValC (ClosureV param body env), env)

-- REGLA PARA Condicional
-- Primero se reduce la condición, propagando el ambiente en caso de que cambie.
-- Si la condición es un valor, si es True la expresión se reduce a thenE,
-- Si es False se reduce a elseE
smallStep (IfC cond thenE elseE) env
  | isValue cond = case exprToValue cond of
                     (BoolV True)  -> (thenE, env)
                     (BoolV False) -> (elseE, env)
                     _ -> error "Condición de If no es booleana tras evaluar"
  | otherwise    = let (cond', env') = smallStep cond env in (IfC cond' thenE elseE, env') 

-- REGLAS PARA Operadores Binarios y Unarios
-- Los operandos se evalúan de izquierda a derecha usando smallStepBinOp y smallStepBinPred
-- Una vez que los operandos ya son valores, se aplica la operación
-- Los errores de tipos son señalados en tiempo de ejecución
smallStep (AddC e1 e2) env = smallStepBinOp e1 e2 AddC (\n1 n2 -> NumC (n1 + n2)) env
smallStep (SubC e1 e2) env = smallStepBinOp e1 e2 SubC (\n1 n2 -> NumC (n1 - n2)) env
smallStep (MulC e1 e2) env = smallStepBinOp e1 e2 MulC (\n1 n2 -> NumC (n1 * n2)) env
smallStep (DivC e1 e2) env = smallStepBinOp e1 e2 DivC (\n1 n2 -> if n2 == 0 then error "División por cero" else NumC (n1 `div` n2)) env
smallStep (ExptC e1 e2) env = smallStepBinOp e1 e2 ExptC (\n1 n2 -> NumC (n1 ^ n2)) env
smallStep (EqC e1 e2) env = smallStepBinPred e1 e2 EqC (\v1 v2 -> BoolC (v1 == v2)) env
smallStep (NeqC e1 e2) env = smallStepBinPred e1 e2 NeqC (\v1 v2 -> BoolC (v1 /= v2)) env
smallStep (LtC e1 e2) env = smallStepBinPred e1 e2 LtC (\v1 v2 -> case (v1, v2) of (NumV n1, NumV n2) -> BoolC (n1 < n2); _ -> error "< requiere números") env
smallStep (GtC e1 e2) env = smallStepBinPred e1 e2 GtC (\v1 v2 -> case (v1, v2) of (NumV n1, NumV n2) -> BoolC (n1 > n2); _ -> error "> requiere números") env
smallStep (LteC e1 e2) env = smallStepBinPred e1 e2 LteC (\v1 v2 -> case (v1, v2) of (NumV n1, NumV n2) -> BoolC (n1 <= n2); _ -> error "<= requiere números") env
smallStep (GteC e1 e2) env = smallStepBinPred e1 e2 GteC (\v1 v2 -> case (v1, v2) of (NumV n1, NumV n2) -> BoolC (n1 >= n2); _ -> error ">= requiere números") env
smallStep (NotC e) env
  | isValue e = case exprToValue e of (BoolV b) -> (BoolC (not b), env); _ -> error "Not requiere booleano"
  | otherwise = let (e', env') = smallStep e env in (NotC e', env') 

smallStep (Add1C e) env
  | isValue e = case exprToValue e of (NumV n) -> (NumC (n + 1), env); _ -> error "Add1 requiere número"
  | otherwise = let (e', env') = smallStep e env in (Add1C e', env')

smallStep (Sub1C e) env
  | isValue e = case exprToValue e of (NumV n) -> (NumC (n - 1), env); _ -> error "Sub1 requiere número"
  | otherwise = let (e', env') = smallStep e env in (Sub1C e', env')

smallStep (SqrtC e) env
  | isValue e = case exprToValue e of (NumV n) -> (NumC (floor (sqrt (fromIntegral n :: Double))), env); _ -> error "Sqrt requiere número"
  | otherwise = let (e', env') = smallStep e env in (SqrtC e', env') 

-- REGLA PARA Pares y Listas
-- Se evalúan los componentes de izquierda a derecha, cuando ya son valores, se crea un
-- PairV y se devuelve como un valor ValC
smallStep (PairC e1 e2) env
  | isValue e1 && isValue e2 = (ValC (PairV (exprToValue e1) (exprToValue e2)), env)
  | isValue e1               = let (e2', env2) = smallStep e2 env in (PairC e1 e2', env2) 
  | otherwise                = let (e1', env1) = smallStep e1 env in (PairC e1' e2, env1) 

-- REGLA PARA proyección del primer elemento de un par
-- Evalúa la expresión interna hasta obtener un valor (par) y devuelve
-- el primer elemento. Arroja un error si no es un par
smallStep (FstC e) env
  | isValue e = case exprToValue e of (PairV v1 _) -> (valueToExpr v1, env); _ -> error "Fst requiere un par"
  | otherwise = let (e', env') = smallStep e env in (FstC e', env') 

-- REGLA PARA proyección del segundo elemento de un par
-- Análogo a FstC, pero devuelve el segundo elemento
smallStep (SndC e) env
  | isValue e = case exprToValue e of (PairV _ v2) -> (valueToExpr v2, env); _ -> error "Snd requiere un par"
  | otherwise = let (e', env') = smallStep e env in (SndC e', env') 

-- REGLA para Aplicación 
-- 1. Si la función es un closure y el argumento ya es un valor, le aplicamos el closure:
-- Extraemos el closure de la función a aplicar, obtenemos el Value del argumento,
-- y se extiende el ambiente con el binding del parámetro.
-- Se usa RestoreC para evaluar el cuerpo de la función bajo el ambiente extendido,
-- y una vez que ya se redujo a un valor, se recupera callerEnv (ambiente en el cual
-- se mandó a llamar la función) como ambiente del resultado.
-- 2. De otro modo, si la función es un closure y el argumento es una lambda, se crea un closure
-- para el argumento con el ambiente actual y seguimos como en el primer caso
-- 3. Si la función es un closure pero el argumento aún no es un valor, se reduce el argumento
-- 4. Si la función aún no es un closure, se reduce la función
smallStep (AppC funExpr argExpr) env
  -- 1) Función lista y argumento ya es valor: aplicar con RestoreC
  | isClosure funExpr && isValue argExpr =
      case exprToValue funExpr of
        (ClosureV param body capturedEnv) ->
          let argVal = exprToValue argExpr
              newEnv = (param, argVal) : capturedEnv
          in (RestoreC body env, newEnv)
        _ -> error "Imposible: isClosure falló (caso valor)"
  -- 2) Función lista y argumento es LamC
  | isClosure funExpr && isLam argExpr =
      case exprToValue funExpr of
        (ClosureV param body capturedEnv) ->
          let (LamC argParam argBody) = argExpr
              argVal = ClosureV argParam argBody env
              newEnv = (param, argVal) : capturedEnv
          in (RestoreC body env, newEnv)
        _ -> error "Imposible: isClosure falló (caso isLam argExpr)"
  -- 3) La función ya es un closure, reducir el argumento
  | isClosure funExpr =
      let (argExpr', env') = smallStep argExpr env
      in (AppC funExpr argExpr', env')
  -- 4) Reducir la función
  | otherwise =
      let (funExpr', env') = smallStep funExpr env
      in (AppC funExpr' argExpr, env')

-- RestoreC e callerEnv indica que evaluamos e bajo otro ambiente. CallerEnv contiene
-- el ambiente de quien hace la llamada, y es el ambiente que debe usarse para
-- el resultado final. Si e ya es un valor se devuelve (e, callerEnv), si no,
-- se reduce e en el ambiente actual
smallStep (RestoreC e callerEnv) env
  | isValue e = (e, callerEnv)
  | otherwise = let (e', env') = smallStep e env in (RestoreC e' callerEnv, env')

-- Caso de error si ValC contiene algo que no es valor final
smallStep (ValC v) env | not (isValueVal v) = error ("Error interno: smallStep encontró ValC con no-valor: " ++ show v)
                       | otherwise = (ValC v, env) 


-- #################################################
-- ##       FUNCIONES AUXILIARES para smallStep   ##
-- #################################################

-- Función que realiza un paso pequeño en operadores binarios aritméticos
-- e1 y e2 son los operandos, builder es el constructor del operador, evalFunc
-- es una función que produce la expresión resultante y env es el ambiente actual
-- Si e1 y e2 son valores pero no son NumV, arroja un error
smallStepBinOp :: ExprC -> ExprC -> (ExprC -> ExprC -> ExprC) -> (Int -> Int -> ExprC) -> Env -> (ExprC, Env)
smallStepBinOp e1 e2 builder evalFunc env
  | isValue e1 && isValue e2 =
      case (exprToValue e1, exprToValue e2) of
        (NumV n1, NumV n2) -> (evalFunc n1 n2, env)
        _ -> error ("Error de tipo en operador binario: " ++ show e1 ++ ", " ++ show e2)
  | isValue e1 =
      let (e2', env2) = smallStep e2 env 
      in (builder e1 e2', env2)          
  | otherwise =
      let (e1', env1) = smallStep e1 env 
      in (builder e1' e2, env1)          

-- Función que realiza un paso pequeño en operadores binarios relacionales
-- Análogo al caso de operadores binarios aritméticos
-- evalFunc toma dos Values y produce la expresión en el core resultante (BoolC)
smallStepBinPred :: ExprC -> ExprC -> (ExprC -> ExprC -> ExprC) -> (Value -> Value -> ExprC) -> Env -> (ExprC, Env)
smallStepBinPred e1 e2 builder evalFunc env
  | isValue e1 && isValue e2 = (evalFunc (exprToValue e1) (exprToValue e2), env)
  | isValue e1 =
      let (e2', env2) = smallStep e2 env 
      in (builder e1 e2', env2)          
  | otherwise =
      let (e1', env1) = smallStep e1 env 
      in (builder e1' e2, env1)          



-- #################################################
-- ##                 FUNCIÓN INTERP              ##
-- #################################################
interp :: ExprC -> Env -> Value
interp e env = go e env
  where
    go currentExpr currentEnv
      | isValue currentExpr && isValueVal (exprToValue currentExpr) =
          exprToValue currentExpr
      | otherwise =
          let (nextExpr, nextEnv) = smallStep currentExpr currentEnv
          in go nextExpr nextEnv