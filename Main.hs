module Main where

import Lexer (lexer)
import Parser (parser) -- Asumiendo que el módulo se llama Parser
import Desugar (desugar, Value(..)) -- Asegúrate de importar Value
import Interp (interp) -- Asegúrate de importar interp

-- Función auxiliar para ejecutar la cadena completa y mostrar el resultado
runTest :: String -> IO ()
runTest code = do
  putStrLn $ "========================================================="
  putStrLn $ "TESTING: " ++ code
  let result = interp (desugar (parser (lexer code))) []
  putStrLn $ "RESULTADO: " ++ show result
  -- Nota: Si usaste un contador de pasos, aquí verías el error controlado 
  -- en lugar de que el sistema se congele.

-- La función principal de Haskell
main :: IO ()
main = do
  putStrLn "=== INICIANDO PRUEBAS DE RECURSIÓN MINILISP ==="
  
  -- SUMA NATURALES (Debe funcionar perfecto)
  runTest "(letrec ((sum-nat (lambda (n) (if (= n 0) 0 (+ n (sum-nat (- n 1))))))) (sum-nat 10))"
  runTest "(letrec ((sum-nat (lambda (n) (if (= n 0) 0 (+ n (sum-nat (- n 1))))))) (sum-nat 100))" -- Verifica eficiencia

  -- FACTORIAL (Debe funcionar perfecto)
  runTest "(letrec ((fact (lambda (n) (if (= n 0) 1 (* n (fact (- n 1))))))) (fact 5))"

  -- FIBONACCI (Inestable, documenta el fallo)
  putStrLn "\n--- PRUEBAS DE FIBONACCI ---"
  runTest "(letrec ((fib (lambda (n) (if (= n 0) 0 (if (= n 1) 1 (+ (fib (- n 1)) (fib (- n 2)))))))) (fib 1))" -- F1 = 1 (Caso base)
  runTest "(letrec ((fib (lambda (n) (if (= n 0) 0 (if (= n 1) 1 (+ (fib (- n 1)) (fib (- n 2)))))))) (fib 7))" -- F2 = 13

  putStrLn "\n--- PRUEBAS DE MAP ---"
  runTest "(letrec((map(lambda(f L) (if(=L nil) nil (pair(f(fst L))(map f (snd L))))))) (map (lambda(x)(+ x 1)) (L 1 2 3 4 5)))" -- Incrementa cada elemento de la lista
  putStrLn "\n=== PRUEBAS COMPLETADAS ==="