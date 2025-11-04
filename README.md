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