module Main where

import Lexer (lexer)
import Parser (parser)
import Desugar (desugar)
import Interp (interp, Value(..), Env) -- Asegúrate que Value y Env se exporten si es necesario
import System.IO (hFlush, stdout)

-- Función para convertir el resultado (Value) a String
-- (Ajusta esto según tu tipo Value exacto)
valueToString :: Value -> String
valueToString (NumV n) = show n
valueToString (BoolV b) = if b then "#t" else "#f"
valueToString (ClosureV _ _ _) = "#<procedure>"
valueToString (PairV v1 v2) = "(" ++ valueToString v1 ++ " . " ++ valueToString v2 ++ ")" -- Ejemplo para pares
valueToString NilV = "()" -- Ejemplo para lista vacía

-- Bucle REPL (Read-Eval-Print Loop)
repl :: Env -> IO ()
repl env = do
  putStr "> "
  hFlush stdout
  line <- getLine
  if line == "(exit)" || line == ":q" -- Agrega formas de salir si quieres
    then putStrLn "Adiós."
    else do
      -- Manejo básico de errores (puedes mejorarlo)
      let result = interp (desugar (parser (lexer line))) env
      putStrLn (valueToString result)
      repl env -- Llama recursivamente con el mismo entorno (o uno actualizado si tuvieras `define`)

-- Función principal
main :: IO ()
main = do
  putStrLn "MiniLisp Intérprete ¡Bienvenidx!"
  -- Puedes añadir definiciones iniciales al entorno si es necesario, como el combinador Y
  -- let initialEnv = [("Y", yCombinatorValue)] -- Define yCombinatorValue
  let initialEnv = [] -- Entorno inicial vacío
  repl initialEnv