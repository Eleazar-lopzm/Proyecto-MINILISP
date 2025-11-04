module Main where

import Lexer (lexer)
import Parser (parser) 
import Desugar (desugar, Value(..)) 
import Interp (interp) 

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
  
  -- Pruebas operaciones básicas
  putStrLn "\n--- PRUEBAS DE OPERACIONES BÁSICAS ---"
  runTest "(+ 1 2)"          -- Suma básica
  runTest "(- 5 3)"          -- Resta básica
  runTest "(* 4 2)"          -- Multiplicación básica
  runTest "(/ 8 2)"          -- División básica
  runTest "(= 3 3)"          -- Igualdad verdadera
  runTest "(= 3 4)"          -- Igualdad falsa
  runTest "(> 5 2)"          -- Mayor que verdadero
  runTest "(> 2 5)"          -- Mayor que falso
  runTest "(>= 5 5)"         -- Mayor o igual que verdadero
  runTest "(>= 5 6)"         -- Mayor o igual que falso
  runTest "(< 2 5)"          -- Menor que verdadero
  runTest "(< 5 2)"          -- Menor que falso
  runTest "(<= 5 5)"         -- Menor o igual que verdadero
  runTest "(<= 5 4)"         -- Menor o igual que falso   

  -- Pruebas de operaciones variádicas
  putStrLn "\n--- PRUEBAS DE OPERACIONES VARIÁDICAS ---"
  runTest "(+ 1 2 3 4 5)"    -- Suma variádica
  runTest "(* 1 2 3 4 5)"    -- Multiplicación variádica
  runTest "(- 20 5 3 2)"     -- Resta variádica
  runTest "(/ 100 2 5)"      -- División variádica
  runTest "(> 1 2 3)"        -- Cadena de mayor que (falso)
  runTest "(< 1 2 3)"        -- Cadena de menor que (verdadero)
  runTest "(>= 3 2 2)"       -- Cadena de mayor o igual que (verdadero)
  runTest "(<= 2 2 3)"       -- Cadena de menor o igual que (verdadero)
  runTest "(= 2 2 2)"        -- Cadena de igualdad (verdadero)
  runTest "(= 2 2 3)"        -- Cadena de igualdad (falso)  

  -- Pruebas de condicionales
  putStrLn "\n--- PRUEBAS DE CONDICIONALES ---"
  runTest "(cond ((> 3 2) 100) ((< 3 2) 200) (else 300))" -- Primer guard verdadero
  runTest "(cond ((< 3 2) 100) ((> 3 2) 200) (else 300))" -- Segundo guard verdadero
  runTest "(cond ((< 3 2) 100) ((= 3 2) 200) (else 300))" -- else tomado

  -- Pruebas not 
  putStrLn "\n--- PRUEBAS DE NOT ---"
  runTest "not(true)"   -- not true
  runTest "not(false)"  -- not false

  -- Pruebas add1 y sub1
  putStrLn "\n--- PRUEBAS DE ADD1 Y SUB1 ---"
  runTest "add1(5)"    -- add1
  runTest "sub1(5)"    -- sub1

  --Pruebas de if
  runTest "(if (> 3 2) 100 200)" -- Condición verdadera
  runTest "(if (< 3 2) 100 200)" -- Condición falsa

  --Pruebas de Pares y Listas
  putStrLn "\n--- PRUEBAS DE PARES Y LISTAS ---"
  runTest "(pair 10 20)"               -- Crear un par
  runTest "(fst (pair 10 20))"         -- Obtener el primer elemento
  runTest "(snd (pair 10 20))"         -- Obtener el segundo elemento
  runTest "(list 1 2 3 4 5)"           -- Crear una lista
  runTest "(head (list 1 2 3 4 5))"     -- Obtener el primer elemento de la lista
  runTest "(tail (list 1 2 3 4 5))"     -- Obtener el resto de la lista

  -- Pruebas de let y let*
  putStrLn "\n--- PRUEBAS DE LET Y LET* ---"
  runTest "(let ((x 10) (y 20)) (+ x y))"  -- let básico
  runTest "(let* ((x 10) (y (+ x 5))) (+ x y))" -- let* con dependencia

  -- Pruebas de funciones y aplicaciones
  putStrLn "\n--- PRUEBAS DE FUNCIONES Y APLICACIONES ---"
  runTest "((lambda (x) (+ x 10)) 5)"        -- Aplicación simple
  runTest "((lambda (x y) (* x y)) 4 5)"     -- Aplicación con múltiples argumentos
  runTest "(((lambda (x) (lambda (y) (+ x y))) 10) 20)" -- Función que retorna otra función
  runTest "((lambda (x) (lambda (y) (lambda (z) (+ x y z)))) 1 2 3)" -- Función curried con tres argumentos

  putStrLn "\n--- PRUEBAS DE RECURSIÓN ---"
  -- SUMA NATURALES 
  runTest "(letrec ((sum-nat (lambda (n) (if (= n 0) 0 (+ n (sum-nat (- n 1))))))) (sum-nat 10))"
  runTest "(letrec ((sum-nat (lambda (n) (if (= n 0) 0 (+ n (sum-nat (- n 1))))))) (sum-nat 100))" -- Verifica eficiencia

  -- FACTORIAL 
  runTest "(letrec ((fact (lambda (n) (if (= n 0) 1 (* n (fact (- n 1))))))) (fact 5))"

  -- FIBONACCI 
  putStrLn "\n--- PRUEBAS DE FIBONACCI ---"
  runTest "(letrec ((fib (lambda (n) (if (= n 0) 0 (if (= n 1) 1 (+ (fib (- n 1)) (fib (- n 2)))))))) (fib 1))" -- F1 = 1 (Caso base)
  runTest "(letrec ((fib (lambda (n) (if (= n 0) 0 (if (= n 1) 1 (+ (fib (- n 1)) (fib (- n 2)))))))) (fib 7))" -- F2 = 13

  -- Pruebas que no funcionan por desborde de pila
  putStrLn "\n--- PRUEBAS DE MAP ---"
  runTest "(letrec((map(lambda(f L) (if(=L nil) nil (pair(f(fst L))(map f (snd L))))))) (map (lambda(x)(+ x 1)) (L 1 2 3 4 5)))" -- Incrementa cada elemento de la lista

  putStrLn "\n--- PRUEBAS DE FILTER ---"
  runTest "(letrec((filter(lambda(pred L) (if(=L nil) nil (if(pred(fst L)) (pair(fst L)(filter pred (snd L))) (filter pred (snd L))))))) (filter (lambda(x)(> x 3)) (L 1 2 3 4 5 6)))" -- Filtra elementos mayores a 3

  putStrLn "\n=== PRUEBAS COMPLETADAS ==="