module Main where
import System.IO (hFlush, stdout)
import Control.Exception (SomeException, try, evaluate)

import Lexer (lexer)
import Parser (parser)
import Desugar (desugar, Value(..))
import Interp (interp)

runTest :: String -> IO ()
runTest code = do
  putStrLn "========================================================="
  putStrLn $ "TESTING: " ++ code
  e <- try (evaluate (show (interp (desugar (parser (lexer code))) []))) :: IO (Either SomeException String)
  case e of
    Left err -> putStrLn $ "ERROR: " ++ show err
    Right s  -> putStrLn $ "RESULTADO: " ++ s

-- Ejecuta un grupo de pruebas con título
runGroup :: String -> [String] -> IO ()
runGroup title tests = do
  putStrLn $ "\n--- " ++ title ++ " ---"
  mapM_ runTest tests

-- Espera Enter para continuar
pause :: IO ()
pause = do
  putStr "Presiona Enter para continuar..."
  hFlush stdout
  _ <- getLine
  pure ()

-- Listas de pruebas
basicOps :: [String]
basicOps =
  [ "(+ 1 2)"
  , "(- 5 3)"
  , "(* 4 2)"
  , "(/ 8 2)"
  , "(= 3 3)"
  , "(= 3 4)"
  , "(> 5 2)"
  , "(> 2 5)"
  , "(>= 5 5)"
  , "(>= 5 6)"
  , "(< 2 5)"
  , "(< 5 2)"
  , "(<= 5 5)"
  , "(<= 5 4)"
  ]

variadicOps :: [String]
variadicOps =
  [ "(+ 1 2 3 4 5)"
  , "(* 1 2 3 4 5)"
  , "(- 20 5 3 2)"
  , "(/ 100 2 5)"
  , "(> 1 2 3)"
  , "(< 1 2 3)"
  , "(>= 3 2 2)"
  , "(<= 2 2 3)"
  , "(= 2 2 2)"
  , "(= 2 2 3)"
  ]

condTests :: [String]
condTests =
  [ "(cond [(> 3 2) 100] [(< 3 2) 200] [else 300])"
  , "(cond [(< 3 2) 100] [(> 3 2) 200] [else 300])"
  , "(cond [(< 3 2) 100] [(= 3 2) 200] [else 300])"
  ]

notTests :: [String]
notTests = [ "(not #t)", "(not #f)" ]

add1sub1Tests :: [String]
add1sub1Tests = [ "(add1 5)", "(sub1 5)" ]

ifTests :: [String]
ifTests =
  [ "(if (> 3 2) 100 200)"
  , "(if (< 3 2) 100 200)"
  ]

pairsLists :: [String]
pairsLists =
  [ "(pair 10 20)"
  , "(fst (pair 10 20))"
  , "(snd (pair 10 20))"
  , "[1 2 3 4 5]"
  , "(head [1 2 3 4 5])"
  , "(tail [1 2 3 4 5])"
  ]

letTests :: [String]
letTests =
  [ "(let ((x 10) (y 20)) (+ x y))"
  , "(let* ((x 10) (y (+ x 5))) (+ x y))"
  ]

funcTests :: [String]
funcTests =
  [ "((lambda (x) (+ x 10)) 5)"
  , "((lambda (x y) (* x y)) 4 5)"
  , "(((lambda (x) (lambda (y) (+ x y))) 10) 20)"
  , "((lambda (x) (lambda (y) (lambda (z) (+ x y z)))) 1 2 3)"
  ]

recSumTests :: [String]
recSumTests =
  [ "(letrec ((sum-nat (lambda (n) (if (= n 0) 0 (+ n (sum-nat (- n 1))))))) (sum-nat 10))"
  , "(letrec ((sum-nat (lambda (n) (if (= n 0) 0 (+ n (sum-nat (- n 1))))))) (sum-nat 100))"
  ]

factTests :: [String]
factTests =
  [ "(letrec ((fact (lambda (n) (if (= n 0) 1 (* n (fact (- n 1))))))) (fact 5))"
  ]

fibTests :: [String]
fibTests =
  [ "(letrec ((fib (lambda (n) (if (= n 0) 0 (if (= n 1) 1 (+ (fib (- n 1)) (fib (- n 2)))))))) (fib 1))"
  , "(letrec ((fib (lambda (n) (if (= n 0) 0 (if (= n 1) 1 (+ (fib (- n 1)) (fib (- n 2)))))))) (fib 7))"
  ]

mapTests :: [String]
mapTests =
  [ "(letrec((map(lambda(f L) (if(=L nil) nil (pair(f(fst L))(map f (snd L))))))) (map (lambda(x)(+ x 1)) [1 2 3 4 5]))"
  ]

filterTests :: [String]
filterTests =
  [ "(letrec((filter(lambda(pred L) (if(=L nil) nil (if(pred(fst L)) (pair(fst L)(filter pred (snd L))) (filter pred (snd L))))))) (filter (lambda(x)(> x 3)) [1 2 3 4 5 6]))"
  ]

runAll :: IO ()
runAll = do
  runGroup "PRUEBAS DE OPERACIONES BÁSICAS" basicOps
  runGroup "PRUEBAS DE OPERACIONES VARIÁDICAS" variadicOps
  runGroup "PRUEBAS DE CONDICIONALES" condTests
  runGroup "PRUEBAS DE NOT" notTests
  runGroup "PRUEBAS DE ADD1 Y SUB1" add1sub1Tests
  runGroup "PRUEBAS DE IF" ifTests
  runGroup "PRUEBAS DE PARES Y LISTAS" pairsLists
  runGroup "PRUEBAS DE LET Y LET*" letTests
  runGroup "PRUEBAS DE FUNCIONES Y APLICACIONES" funcTests
  runGroup "PRUEBAS DE RECURSIÓN (SUMA NATURALES)" recSumTests
  runGroup "PRUEBAS DE FACTORIAL" factTests
  runGroup "PRUEBAS DE FIBONACCI" fibTests
  runGroup "PRUEBAS DE MAP" mapTests
  runGroup "PRUEBAS DE FILTER" filterTests

menu :: IO ()
menu = do
  putStrLn "\n=== MENÚ DE PRUEBAS MINI-LISP ==="
  putStrLn "1) Operaciones básicas"
  putStrLn "2) Operaciones variádicas"
  putStrLn "3) Condicionales (cond)"
  putStrLn "4) not"
  putStrLn "5) add1 y sub1"
  putStrLn "6) if"
  putStrLn "7) Pares y listas"
  putStrLn "8) let y let*"
  putStrLn "9) Funciones y aplicaciones (lambda)"
  putStrLn "10) Recursión: suma de naturales"
  putStrLn "11) Recursión: factorial"
  putStrLn "12) Fibonacci"
  putStrLn "13) Map"
  putStrLn "14) Filter"
  putStrLn "15) Ejecutar TODAS las pruebas"
  putStrLn "16) Escribe tu operación"
  putStrLn "0) Salir"
  putStr "Selecciona una opción: "
  hFlush stdout

loop :: IO ()
loop = do
  menu
  opt <- getLine
  case opt of
    "1"  -> runGroup "PRUEBAS DE OPERACIONES BÁSICAS" basicOps >> pause >> loop
    "2"  -> runGroup "PRUEBAS DE OPERACIONES VARIÁDICAS" variadicOps >> pause >> loop
    "3"  -> runGroup "PRUEBAS DE CONDICIONALES" condTests >> pause >> loop
    "4"  -> runGroup "PRUEBAS DE NOT" notTests >> pause >> loop
    "5"  -> runGroup "PRUEBAS DE ADD1 Y SUB1" add1sub1Tests >> pause >> loop
    "6"  -> runGroup "PRUEBAS DE IF" ifTests >> pause >> loop
    "7"  -> runGroup "PRUEBAS DE PARES Y LISTAS" pairsLists >> pause >> loop
    "8"  -> runGroup "PRUEBAS DE LET Y LET*" letTests >> pause >> loop
    "9"  -> runGroup "PRUEBAS DE FUNCIONES Y APLICACIONES" funcTests >> pause >> loop
    "10" -> runGroup "PRUEBAS DE RECURSIÓN (SUMA NATURALES)" recSumTests >> pause >> loop
    "11" -> runGroup "PRUEBAS DE FACTORIAL" factTests >> pause >> loop
    "12" -> runGroup "PRUEBAS DE FIBONACCI" fibTests >> pause >> loop
    "13" -> runGroup "PRUEBAS DE MAP" mapTests >> pause >> loop
    "14" -> runGroup "PRUEBAS DE FILTER" filterTests >> pause >> loop
    "15" -> runAll >> pause >> loop
    "16" -> do
              putStr "Escribe tu operación: "
              hFlush stdout
              code <- getLine
              runTest code
              pause
              loop
    "0"  -> putStrLn "Saliendo..."
    _    -> putStrLn "Opción inválida." >> pause >> loop

-- La función principal
main :: IO ()
main = do
  loop