# APP3-El Bosque de las Runas Magicas

### Lenguajes y Paradigmas de Programación, Sección 2, Santiago
- José Vicente Cornejo [josevcornejo@alumnos.uai.cl]
- Ignacio Arau [iarau@alumnos.uai.cl]
- Ignacio Oyarzún [ignoyarzun@alumnos.uai.cl]
- Maximiliano Rojas [maximrojas@alumnos.uai.cl]


## 1. Descripción del Proyecto
Este proyeto, "El Bosque de las Runas Mágicas", es la App #3 del curso TICS200, enfocada en la aplicación del Paradigma de Programación Funcional utilizando Haskell. El ojetivo principal es guiar a un mago a través de un bosque representado como una matríz de runas, desde la esquina superior izquierda hasta la inferior derecha, buscando maximizar su energía restante.

Cada runa en el bosque modifica la energía del mago (sumando, restando, o aplicando efectos especiales). El mago tiene una serie de movimientos permitidos, incluyendo movimientos de una casilla (derecha, abajo, diagonal, izquierda/arriba sin revisitar) y movimientos especiales activados por runas específicas.

Requerimientos Funcionales Clave:
El bosque es una matriz de enteros (representados como String en el código para incluir runas especiales).

Movimientos: Derecha, Abajo, Diagonal abajo-derecha (costo +2 energía), Izquierda/Arriba (si no son celdas ya visitadas).

Runa "0": Trampa, pierde 3 puntos de energía adicionales.

En cada celda, se suma/resta el valor de la runa a la energía.

El mago comienza con una energía inicial (por defecto 12).

Si la energía es menor que 0 en cualquier momento, el camino se invalida.

El programa debe encontrar el camino que deje al mago con la mayor energía posible al final.

Bonus Implementados:
Runa "D" (Doble Salto): Si el mago se encuentra sobre una runa "D", puede realizar movimientos de doble salto (2 casillas en lugar de 1, en las direcciones derecha, abajo o diagonal abajo-derecha). El costo de energía para estos movimientos se aplica según si son rectos (1) o diagonales (2).

Runa "T" (Teletransportador): Si el mago se encuentra sobre una runa "T" y tiene suficiente energía (por defecto, al menos 10), puede teletransportarse a una coordenada fija (por defecto, (0,0), el inicio del bosque) con un costo de energía específico (por defecto, 5).

## 2. Cómo Compilar y Ejecutar
Para compilar y ejecutar este programa, necesitarás tener instalado el compilador de Haskell, GHC (Glasgow Haskell Compiler), en tu sistema. Se recomienda usar ghcup para su instalación.

# 2.1. Compilación Local:
Guarda el código: Asegúrate de que el archivo Main.hs (con el código proporcionado en el Canvas) esté en el directorio raíz de tu proyecto.

Abre una terminal: Navega hasta el directorio donde se encuentra Main.hs.

Compila el programa:

ghc Main.hs

Esto generará un archivo ejecutable (Main.exe en Windows o Main en Linux/macOS) en el mismo directorio.

# 2.2. Ejecución Local:
Desde la terminal:

.\Main.exe


El programa imprimirá el mejor camino encontrado (una lista de coordenadas) y la energía final resultante en la consola.

# 2.3. Integración Continua (CI) con GitHub Actions:
El repositorio incluye un flujo de trabajo de GitHub Actions (.github/workflows/compilar.yml) que compila automáticamente el proyecto cada vez que se suben cambios (push) a la rama main o se crea un pull request. Esto asegura que el código siempre se mantenga compilable. Puedes ver el estado de estas ejecuciones en la pestaña "Actions" de tu repositorio en GitHub.

## 3. Uso de Inteligencia Artificial (IA)
Para este proyeto, se utilizó la Inteligencia Artificial de manera efectiva como una herramienta de apoyo en varias fases del desarrollo. La IA fue fundamental para la depuración de errores específicos, para obtener aclaraciones sobre la sintaxis de Haskell que se nos olvidava, y para generar ideas y enfoques en la implementación de partes clave del código, como la lógica para la runa 'T' o el manejo de los movimientos diagonales. El equipo guió activamente a la IA, seleccionando y refinando las sugerencias para asegurar que se alinearan con el paradigma funcional y los objetivos del proyecto. Toda la lógica central y el diseño de la solución fueron concebidos por el propio equipo, validando cada contribución de la IA mediante pruebas exhaustivas para garantizar su correcto funcionamiento y cohesión con el resto del código.