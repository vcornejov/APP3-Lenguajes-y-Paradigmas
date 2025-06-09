-- | Módulo: Main
-- | Descripción: App #3 - El Bosque de las Runas Mágicas.
-- | Este módulo implementa la lógica para encontrar el camino de máxima energía en un bosque de runas,
-- | siguiendo el paradigma de Programación Funcional en Haskell. Incluye el manejo de runas especiales
-- | como la runa "0" (trampa) y la runa "D" (doble salto).

module Main where -- Define el módulo principal. En Haskell, 'Main' es el punto de entrada del programa.

-- 1. Importaciones:
-- Importamos las funciones necesarias de las librerías estándar de Haskell.
import Data.List (maximumBy) -- Usado para encontrar el elemento máximo en una lista basado en un criterio.
import Data.Ord (comparing)   -- Usado junto con 'maximumBy' para comparar elementos por un campo específico.

-- 2. Definiciones de Tipos de Datos y Sinónimos de Tipo:
-- Definir tipos personalizados y sinónimos de tipo mejora la legibilidad, mantenibilidad
-- y la seguridad de tipos, que son principios importantes en Haskell.
type Coordinate = (Int, Int) -- Representa una posición (fila, columna) en el bosque.
type Forest = [[String]]     -- El bosque se representa como una matriz de cadenas (runas).
type Path = [Coordinate]     -- Un camino es una lista de coordenadas visitadas.

-- WizardState es un tipo de dato que encapsula el estado completo del mago en un momento dado.
-- Es crucial para el paradigma funcional ya que permite el manejo del estado de forma inmutable:
-- en lugar de modificar un estado existente, creamos una nueva instancia de WizardState con los cambios.
data WizardState = WizardState
    { wsCurrentEnergy :: Int        -- La energía actual del mago.
    , wsCurrentPath   :: Path       -- El camino recorrido hasta el momento.
    , wsVisitedCells  :: [Coordinate] -- Lista de todas las celdas visitadas para evitar ciclos (requerimiento c y d).
    } deriving (Show, Eq) -- Derivamos instancias de Show (para imprimir) y Eq (para comparar).

-- 3. Constantes:
-- Valores fijos que no cambian a lo largo de la ejecución del programa.
-- Promueve la inmutabilidad y la claridad del código.
initialEnergy :: Int
initialEnergy = 12 -- Energía con la que el mago comienza su aventura.

-- 4. Funciones de Lógica Central (Puramente Funcionales):
-- Aquí reside la esencia del paradigma funcional:
-- * Pureza: Cada función produce la misma salida para las mismas entradas y no causa efectos secundarios (modificaciones externas).
-- * Recursión: Se utiliza la recursión para iterar o procesar estructuras de datos, en lugar de bucles imperativos.
-- * Funciones de Orden Superior: Se utilizan funciones que toman otras funciones como argumentos o devuelven funciones (ej. `map`, `filter`).

-- | Calcula el efecto de una runa en la energía actual del mago.
-- Aplica las reglas específicas para la runa "0" (trampa) y "D" (doble salto).
calculateRuneEffect :: Int -> String -> Int
calculateRuneEffect currentEnergy runeValue
    | runeValue == "0" = currentEnergy - 3 -- Si la runa es "0", es una trampa: el mago pierde 3 puntos de energía adicionales.
    | runeValue == "D" = currentEnergy     -- La runa 'D' en la celda de destino no cambia la energía directamente;
                                           -- su efecto es habilitar el doble salto desde esa celda.
    | otherwise        = currentEnergy + read runeValue -- Para cualquier otra runa, su valor numérico
                                                         -- (positivo o negativo) se suma/resta a la energía actual.

-- | Genera una lista de todas las coordenadas a las que el mago puede moverse desde su posición actual.
-- Considera los límites del bosque y las reglas de movimiento, incluyendo la runa "D".
generatePossibleMoves :: Forest -> Coordinate -> Path -> [Coordinate]
generatePossibleMoves forest (row, col) visitedCells =
    let numRows = length forest                 -- Número total de filas en el bosque.
        numCols = length (head forest)          -- Número total de columnas (asumiendo que es rectangular).
        -- Función auxiliar que verifica si una coordenada dada está dentro de los límites del bosque.
        isValidCoord (r, c) = r >= 0 && r < numRows && c >= 0 && c < numCols
        -- Función auxiliar que verifica si una celda no ha sido visitada previamente (para movimientos 'izquierda'/'arriba').
        notVisited c = c `notElem` visitedCells

        -- Obtiene el valor de la runa en la celda actual del mago.
        currentRune = (forest !! row) !! col

        -- Define los movimientos de una sola casilla: derecha, abajo, diagonal abajo-derecha, izquierda, arriba.
        singleMoves =
            let rightMove = (row, col + 1)     -- Movimiento a la derecha.
                downMove  = (row + 1, col)     -- Movimiento hacia abajo.
                diagonalMove = (row + 1, col + 1) -- Movimiento diagonal abajo-derecha.
                leftMove = (row, col - 1)      -- Movimiento a la izquierda.
                upMove = (row - 1, col)        -- Movimiento hacia arriba.
            in filter isValidCoord (          -- Filtra para asegurar que los movimientos estén dentro de los límites del bosque.
                [rightMove, downMove, diagonalMove] ++ -- Movimientos siempre permitidos.
                (if notVisited leftMove then [leftMove] else []) ++ -- Movimiento izquierda solo si no visitada.
                (if notVisited upMove then [upMove] else [])       -- Movimiento arriba solo si no visitada.
               )

        -- Define los movimientos de doble salto (desde una runa 'D'): doble derecha, doble abajo, doble diagonal abajo-derecha.
        doubleMoves =
            let right2 = (row, col + 2) -- Doble salto a la derecha.
                down2  = (row + 2, col) -- Doble salto hacia abajo.
                diag2  = (row + 2, col + 2) -- Doble salto diagonal abajo-derecha.
            in filter isValidCoord [right2, down2, diag2] -- Filtra para asegurar que estén dentro de los límites.

    in
    -- Si la runa en la celda actual es "D", el mago puede realizar movimientos de doble salto.
    -- De lo contrario, solo puede realizar los movimientos de una sola casilla.
    if currentRune == "D"
        then doubleMoves
        else singleMoves

-- | Determina si un movimiento del mago fue diagonal.
-- Un movimiento se considera diagonal si tanto la fila como la columna cambian,
-- ya sea por un paso (1,1) o por un doble salto (2,2).
isDiagonalMove :: Coordinate -> Coordinate -> Bool
isDiagonalMove (r1, c1) (r2, c2) =
    let distR = abs (r1 - r2) -- Diferencia absoluta en filas.
        distC = abs (c1 - c2) -- Diferencia absoluta en columnas.
    in (distR == 1 && distC == 1) || (distR == 2 && distC == 2) -- Es diagonal si ambos cambian por 1 o por 2.

-- | Determina si un movimiento fue un "doble salto".
-- Un doble salto ocurre si la distancia en filas o columnas (o ambas para diagonal) es 2.
isDoubleStep :: Coordinate -> Coordinate -> Bool
isDoubleStep (r1, c1) (r2, c2) =
    let distR = abs (r1 - r2) -- Diferencia absoluta en filas.
        distC = abs (c1 - c2) -- Diferencia absoluta en columnas.
    in (distR == 2 && distC == 0) || (distR == 0 && distC == 2) || (distR == 2 && distC == 2) -- Doble salto en X, Y o diagonal.

-- | Actualiza el estado del mago (energía, camino, celdas visitadas) después de realizar un movimiento.
-- Esta es una función pura: toma un estado de mago y una nueva coordenada, y devuelve un *nuevo* estado.
applyMove :: Forest -> WizardState -> Coordinate -> WizardState
applyMove forest (WizardState currentE currentP visitedC) nextCoord@(r, c) =
    let
        prevCoord@(pr, pc) = last currentP -- Obtiene la coordenada anterior del camino.
        runeValueAtNextCoord = (forest !! r) !! c -- Obtiene el valor de la runa en la celda de destino.

        -- Calcula la energía después de aplicar el efecto de la runa en la celda de destino.
        energyAfterRuneEffect = calculateRuneEffect currentE runeValueAtNextCoord

        -- Determina el costo de energía del movimiento.
        -- Los movimientos diagonales (ya sean de un paso o doble salto diagonal) cuestan 2 unidades de energía.
        -- Los movimientos rectos (horizontales o verticales, de un paso o doble salto) cuestan 1 unidad de energía.
        moveCost =
            if isDiagonalMove prevCoord nextCoord
                then 2 -- Costo para movimientos diagonales.
                else 1 -- Costo para movimientos horizontales o verticales.

        -- Calcula la energía final después de restar el costo del movimiento.
        energyAfterMoveCost = energyAfterRuneEffect - moveCost

        -- Identifica la coordenada intermedia que se "salta" si el movimiento fue un doble salto.
        -- Esta celda también debe marcarse como visitada para cumplir las reglas.
        intermediateCoord =
            if isDoubleStep prevCoord nextCoord
                -- Calcula la coordenada intermedia (punto medio entero).
                then Just ((pr + r) `div` 2, (pc + c) `div` 2)
                else Nothing -- No hay celda intermedia para movimientos de un solo paso.

        -- Crea la nueva lista de celdas visitadas, añadiendo la celda de destino y la intermedia (si existe).
        newVisited = visitedC ++ [nextCoord] ++ (case intermediateCoord of Just coord -> [coord]; Nothing -> [])

        -- Extiende el camino actual con la nueva coordenada de destino.
        newPath = currentP ++ [nextCoord]
    in
    -- Retorna un nuevo estado de mago con la energía actualizada, el camino extendido y las celdas visitadas actualizadas.
    WizardState energyAfterMoveCost newPath newVisited

-- | Función principal recursiva que encuentra todos los caminos válidos desde el estado actual del mago
-- hasta el destino final. Para cada camino válido, devuelve una tupla que contiene el camino y la energía final.
-- Esta función implementa el algoritmo de búsqueda de caminos utilizando recursión.
findPaths :: Forest -> WizardState -> [(Path, Int)]
findPaths forest (WizardState currentEnergy currentPath visitedCells)
    | currentEnergy < 0 = [] -- Condición de poda: Si la energía del mago cae por debajo de 0 en cualquier punto,
                             -- este camino se considera inválido y se descarta.
    | currentCoordinate == (targetRow, targetCol) = [(currentPath, currentEnergy)] -- Caso base: Si el mago ha llegado
                                                                                   -- a la esquina inferior derecha (destino),
                                                                                   -- el camino actual es válido. Se devuelve el camino
                                                                                   -- y la energía final en ese punto.
    | otherwise = concatMap (findPaths forest) nextWizardStates -- Caso recursivo: Si no ha llegado al destino y la energía
                                                                 -- es suficiente, explora los posibles movimientos.
    where
        currentCoordinate = last currentPath -- La posición actual del mago.
        targetRow = length forest - 1        -- La fila de la celda de destino.
        targetCol = length (head forest) - 1 -- La columna de la celda de destino.

        -- Genera una lista de todas las coordenadas a las que el mago puede moverse desde la celda actual.
        possibleNextMoves = generatePossibleMoves forest currentCoordinate visitedCells

        -- Para cada movimiento posible, crea un nuevo estado de mago aplicando ese movimiento.
        -- Luego, llama recursivamente a 'findPaths' con cada uno de estos nuevos estados para seguir explorando.
        -- 'concatMap' aplana la lista de listas de resultados en una sola lista de tuplas (camino, energía).
        nextWizardStates = map (applyMove forest (WizardState currentEnergy currentPath visitedCells)) possibleNextMoves

-- | Encuentra el mejor camino de una lista de caminos, seleccionando aquel con la energía final más alta.
-- Esta función es pura y utiliza 'maximumBy' con 'comparing' para una selección eficiente.
findBestPath :: [(Path, Int)] -> (Path, Int)
findBestPath pathsWithEnergy =
    -- 'maximumBy' toma una función de comparación y una lista, y devuelve el elemento "máximo".
    -- 'comparing snd' crea una función de comparación que compara elementos basándose en su segundo componente (la energía final).
    maximumBy (comparing snd) pathsWithEnergy

-- 5. Función Principal (Main):
-- El punto de entrada del programa. Coordina la inicialización, la búsqueda de caminos y la salida de resultados.
main :: IO ()
main = do
    -- Define la matriz del bosque de runas.
    -- Se ha actualizado con la runa "D" en (4,2) según el enunciado.
    let forest = [[ "2", "-3",  "1",  "0",  "2",  "3"],
                  ["-5",  "4", "-2",  "1",  "0", "-4"],
                  [ "1",  "3",  "0", "-3",  "2",  "2"],
                  [ "2", "-1",  "4",  "0", "-5",  "1"],
                  [ "0",  "2", "D",  "3",  "4",  "1"],
                  [ "1",  "0",  "2", "-2",  "1",  "5"]]

    -- Define la coordenada de inicio del mago (esquina superior izquierda).
    let startCoord = (0, 0)

    -- Inicializa el estado del mago:
    -- - Energía inicial.
    -- - El camino comienza con solo la coordenada de inicio.
    -- - Las celdas visitadas incluyen solo la coordenada de inicio.
    let initialWizardState = WizardState initialEnergy [startCoord] [startCoord]

    putStrLn "Buscando el mejor camino en el bosque de runas..."

    -- Llama a la función principal para encontrar todos los caminos válidos desde el estado inicial.
    -- 'allValidPathsWithEnergy' contendrá una lista de tuplas, donde cada tupla es (camino, energía_final).
    let allValidPathsWithEnergy = findPaths forest initialWizardState

    -- Verifica si se encontró al menos un camino válido.
    if null allValidPathsWithEnergy
        then putStrLn "No se encontró ningún camino válido para llegar al destino."
        else do
            -- Si hay caminos válidos, encuentra el mejor camino (el que resulta en la mayor energía final).
            let (bestPath, finalEnergy) = findBestPath allValidPathsWithEnergy

            -- Imprime los resultados al usuario.
            putStrLn "\n--- Resultado Final ---"
            putStrLn "Mejor Camino Encontrado:"
            print bestPath -- Imprime la lista de coordenadas que forman el camino.
            putStrLn $ "Energía Final: " ++ show finalEnergy -- Imprime la energía final alcanzada.
