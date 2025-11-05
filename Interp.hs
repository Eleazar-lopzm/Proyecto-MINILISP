-- #################################################
-- ## Archivo: Interp.hs (Simplificado sin LetC)  ##
-- #################################################
module Interp (interp, Value(..), Env) where

import Desugar (ExprC(..), Value(..), Env, Id)
import Prelude hiding (lookup)
import Debug.Trace (trace) 

-- #################################################
-- ##       FUNCIONES isValue, lookup, etc.       ##
-- #################################################

isValue :: ExprC -> Bool
isValue (NumC _)  = True
isValue (BoolC _) = True
isValue NilC      = True
isValue (ValC v)  = isValueVal v
isValue _         = False

isValueVal :: Value -> Bool
isValueVal (PairV v1 v2) = isValueVal v1 && isValueVal v2
isValueVal _             = True

lookup :: Id -> Env -> Value
lookup var [] = error ("Variable libre: " ++ var)
lookup var ((name, val):rest)
  | var == name = val
  | otherwise   = lookup var rest

isLam :: ExprC -> Bool
isLam (LamC _ _) = True
isLam _          = False

valueToExpr :: Value -> ExprC
valueToExpr (NumV n) = NumC n
valueToExpr (BoolV b) = BoolC b
valueToExpr NilV = NilC
valueToExpr v@(ClosureV _ _ _) = ValC v
valueToExpr v@(PairV _ _) = ValC v

exprToValue :: ExprC -> Value
exprToValue (NumC n) = NumV n
exprToValue (BoolC b) = BoolV b
exprToValue NilC = NilV
exprToValue (ValC v) = v
exprToValue e = error ("Intentando convertir ExprC no-valor a Value: " ++ show e)

isClosure :: ExprC -> Bool
isClosure (ValC (ClosureV _ _ _)) = True
isClosure _                       = False

-- #################################################
-- ##       FUNCIÓN PRINCIPAL: smallStep          ##
-- #################################################
smallStep :: ExprC -> Env -> (ExprC, Env)

-- Reglas para Valores y Variables 
smallStep e@(NumC _) env  = (e, env)
smallStep e@(BoolC _) env = (e, env)
smallStep e@NilC env      = (e, env)
smallStep e@(ValC v) env | isValueVal v = (e, env)
smallStep (IdC i) env = (ValC (lookup i env), env)

-- Regla para Funciones 
smallStep (LamC param body) env = trace ("Creating closure for LamC. CapturedEnv: " ++ show env) $ (ValC (ClosureV param body env), env)

-- --- ¡CASO LetC ELIMINADO! ---
-- La lógica para LetS, LetStarS y LetRecS ahora es
-- manejada por AppC y LamC, gracias al desazucarado.

-- Regla para If
smallStep (IfC cond thenE elseE) env
  | isValue cond = case exprToValue cond of
                     (BoolV True)  -> (thenE, env)
                     (BoolV False) -> (elseE, env)
                     _ -> error "Condición de If no es booleana tras evaluar"
  | otherwise    = let (cond', env') = smallStep cond env in (IfC cond' thenE elseE, env') 

-- Reglas para Operadores Binarios y Unarios 
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

-- Reglas para Pares y Listas (sin cambios)
smallStep (PairC e1 e2) env
  | isValue e1 && isValue e2 = (ValC (PairV (exprToValue e1) (exprToValue e2)), env)
  | isValue e1               = let (e2', env2) = smallStep e2 env in (PairC e1 e2', env2) 
  | otherwise                = let (e1', env1) = smallStep e1 env in (PairC e1' e2, env1) 

smallStep (FstC e) env
  | isValue e = case exprToValue e of (PairV v1 _) -> (valueToExpr v1, env); _ -> error "Fst requiere un par"
  | otherwise = let (e', env') = smallStep e env in (FstC e', env') 

smallStep (SndC e) env
  | isValue e = case exprToValue e of (PairV _ v2) -> (valueToExpr v2, env); _ -> error "Snd requiere un par"
  | otherwise = let (e', env') = smallStep e env in (SndC e', env') 

-- Regla para Aplicación 
smallStep (AppC funExpr argExpr) env
  -- 1) Función lista y argumento ya es valor: aplicar con RestoreC
  | isClosure funExpr && isValue argExpr =
      case exprToValue funExpr of
        (ClosureV param body capturedEnv) ->
          let argVal = exprToValue argExpr
              newEnv = (param, argVal) : capturedEnv
          in trace "Applying closure..." (RestoreC body env, newEnv)
        _ -> error "Imposible: isClosure falló (caso valor)"
  -- 2) Función lista y argumento es LamC: crear cierre del arg con env del llamador (evita ambientes recursivos)
  | isClosure funExpr && isLam argExpr =
      case exprToValue funExpr of
        (ClosureV param body capturedEnv) ->
          let (LamC argParam argBody) = argExpr
              argVal = ClosureV argParam argBody env
              newEnv = (param, argVal) : capturedEnv
          in trace "Applying closure (arg was LamC)..." (RestoreC body env, newEnv)
        _ -> error "Imposible: isClosure falló (caso isLam argExpr)"
  -- 3) La función ya es un cierre, reducir el argumento
  | isClosure funExpr =
      let (argExpr', env') = smallStep argExpr env
      in (AppC funExpr argExpr', env')
  -- 4) Reducir la función
  | otherwise =
      let (funExpr', env') = smallStep funExpr env
      in (AppC funExpr' argExpr, env')

-- Regla para RestoreC: evalúa su subexpresión con el env actual; cuando
-- la subexpresión es valor, restaura el env del llamador guardado.
smallStep (RestoreC e callerEnv) env
  | isValue e = (e, callerEnv)
  | otherwise = let (e', env') = smallStep e env in (RestoreC e' callerEnv, env')

-- Caso de error si ValC contiene algo que no es valor final
smallStep (ValC v) env | not (isValueVal v) = error ("Error interno: smallStep encontró ValC con no-valor: " ++ show v)
                       | otherwise = (ValC v, env) -- Si es valor, no hace nada


-- #################################################
-- ##       FUNCIONES AUXILIARES para smallStep   ##
-- #################################################
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
-- ##       FUNCIÓN INTERP (Itera smallStep)      ##
-- #################################################
interp :: ExprC -> Env -> Value
interp e env = go e env
  where
    go currentExpr currentEnv
      -- Condición de parada: La expresión es un valor Y el *valor* interno es final
      | isValue currentExpr && isValueVal (exprToValue currentExpr) =
          exprToValue currentExpr
      | otherwise =
          let (nextExpr, nextEnv) = smallStep currentExpr currentEnv
          in go nextExpr nextEnv