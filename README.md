# Proyecto 1: Implementación Formal de MINILISP (Máquina CE-SOS)

## Descripción del Proyecto

Implementación de un intérprete para MINILISP, un lenguaje de programación de estilo funcional puro (LISP), con sintaxis similar a Racket. El objetivo principal es aplicar los fundamentos de la Teoría de Lenguajes de Programación para construir un sistema semánticamente robusto.

El intérprete se construye en Haskell y utiliza las siguientes bases formales:

- Sintaxis Concreta: Definida mediante Alex (Análisis Léxico) y Happy (Análisis Sintáctico).

- Semántica Dinámica: Modelada mediante Semántica Operacional Estructural (SOS) de Paso Pequeño (→).

- Alcance: Alcance Estático (Lexical Scoping), implementado mediante Cerraduras (Closures).

- Recursión: Implementada mediante el Combinador Z (Punto Fijo para Call-by-Value).

## Ejecución 
En la terminal de su preferencia introduzca el siguiente comando y siga las instrucciones del menú.
``` bash 

    runhaskell Main.hs

```

## Uso del Menú

El menú está diseñado para probar las diferentes fases de la formalización(mediante el uso de opciones númericas), desde las operaciones básicas (que prueban la Semántica Operacional Directa) hasta las funciones de alto nivel (que prueban el Desazucarado y las Clausuras).
### Opciones y Descripción de las Pruebas
- 1-7	Pruebas Básicas y Variádicas:	Ejecuta conjuntos de operaciones aritméticas, relacionales (>,<=, =), y estructuras de datos (pair, head, tail).
- 8	let y let*:	Verifica que el desazucarado de la ligadura de variables a λ-aplicaciones es correcto.
- 9	Funciones y Aplicaciones:	Prueba la Currificación y la aplicación de Clausuras con múltiples argumentos (((lambda (x y) ...) 4 5)).
- 10-12	Recursión (letrec):	Pruebas críticas que verifican el Combinador Z y el Alcance Estático (sum-nat, factorial, fibonacci).
- 13-14	map y filter: Pruebas de Funciones de Orden Superior que usan recursión sobre listas (pair/nil).
- 15	Ejecutar TODAS las pruebas:	Ejecuta todos los grupos en secuencia.
- 16	Escribe tu operación: Permite ingresar una única línea de código MiniLisp para una prueba directa de la cadena de compilación (lexer → parser → desugar → interp).
- 0	Salir: Cierra el intérprete.