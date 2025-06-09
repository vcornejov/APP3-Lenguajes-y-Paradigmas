-- | Módulo: Main
-- | Descripción: App #3 - El Bosque de las Runas Mágicas, Contiene una runa "D" especial
-- | Este módulo contiene toda la lógica para encontrar el camino de máxima energía en un bosque de runas.

module Main where -- Define el módulo principal. Para un solo archivo, 'Main' es lo más común.

-- 1. Importaciones:
-- Importa solo lo estrictamente necesario.
import Data.List (maximumBy) -- Para encontrar el elemento máximo en una lista 
import Data.Ord (comparing)   -- Para comparar elementos basándose en una función

-- 2. Definiciones de Tipos de Datos y Sinónimos de Tipo:
-- Esto mejora la legibilidad y la seguridad de tipos, conceptos importantes en Haskell.
type Coordinate = (Int, Int)
type Forest = [[String]]
type Path = [Coordinate]

-- WizardState es clave para la inmutabilidad y el manejo de estado.
-- En vez de modificar variables, creamos nuevas instancias de WizardState.
data WizardState = WizardState
    { wsCurrentEnergy :: Int
    , wsCurrentPath   :: Path
    , wsVisitedCells  :: [Coordinate] -- Necesario para las reglas de movimiento "no visitadas" 
    } deriving (Show, Eq)

-- 3. Constantes:
-- Valores fijos que no cambian, promoviendo la inmutabilidad.
initialEnergy :: Int
initialEnergy = 12 -- Energía inicial del mago 

-- 4. Funciones de Lógica Central (Puramente Funcionales):
-- Aquí reside la mayor parte de la evaluación del paradigma funcional:
-- * Cada función debe ser pura: su salida depende solo de sus entradas y no tiene efectos secundarios.
-- * Se utiliza la recursión en lugar de bucles.
-- * Se aplican funciones de orden superior como `map` y `filter`.

-- | Calcula el efecto de una runa en la energía.
calculateRuneEffect :: Int -> String -> Int
calculateRuneEffect currentEnergy runeValue
    | runeValue == "0" = currentEnergy - 3 -- Trampa: -3 energía adicional 
    | runeValue == "D" = currentEnergy
    | otherwise      = currentEnergy + read runeValue     -- Suma/resta el valor de la runa 

-- | Genera una lista de movimientos posibles desde una coordenada.
-- Considera los límites del bosque y las reglas específicas de los movimientos.
generatePossibleMoves :: Forest -> Coordinate -> Path -> [Coordinate]
generatePossibleMoves forest (row, col) visitedCells =
    let numRows = length forest
        numCols = length (head forest)
        -- Funciones auxiliares para verificar si una coordenada está dentro de los límites
        isValidCoord (r, c) = r >= 0 && r < numRows && c >= 0 && c < numCols
        -- Función para verificar si un movimiento 'izquierda' o 'arriba' vuelve a una celda visitada
        notVisited (r, c) = (r, c) `notElem` visitedCells
        currentRune = (forest !! row) !! col

        -- Movimientos de una casilla
        normalMoves =
            let rightMove = (row, col + 1)
                downMove  = (row + 1, col)
                diagonalMove = (row + 1, col + 1)
                leftMove = (row, col - 1)
                upMove = (row - 1, col)
            in filter isValidCoord (
                [rightMove, downMove, diagonalMove] ++
                (if notVisited leftMove then [leftMove] else []) ++
                (if notVisited upMove then [upMove] else [])
            )
        -- Movimientos de dos casillas
        doubleMoves =
            let right2 = (row, col + 2)
                down2  = (row + 2, col)
                diag2  = (row + 2, col + 2)
            in filter isValidCoord [right2, down2, diag2]

    in
    if currentRune == "D"
        then doubleMoves
        else normalMoves

-- | Determina si un movimiento es diagonal.
isDiagonalMove :: Coordinate -> Coordinate -> Bool
isDiagonalMove (r1, c1) (r2, c2) = abs (r1 - r2) == 1 && abs (c1 - c2) == 1

-- | Actualiza el estado del mago para un movimiento dado.
-- Es una función pura que devuelve un nuevo WizardState.
applyMove :: Forest -> WizardState -> Coordinate -> WizardState
applyMove forest (WizardState currentE currentP visitedC) nextCoord@(r, c) =
    let
        prevCoord@(pr, pc) = last currentP
        rune = (forest !! r) !! c

        isDoubleStep =
            let distR = abs (r - pr)
                distC = abs (c - pc)
            in rune == "D" && ((distR == 2 && distC == 0) || (distR == 0 && distC == 2) || (distR == 2 && distC == 2))

        skippedCoord = if isDoubleStep then ((pr + r) `div` 2, (pc + c) `div` 2) else (-1, -1)

        energyAfterRune =
            if isDoubleStep
                then currentE
                else calculateRuneEffect currentE rune

        energyAfterMoveCost =
            if isDiagonalMove prevCoord nextCoord
                then energyAfterRune - 2
                else energyAfterRune - 1

        newVisited =
            if isDoubleStep
                then visitedC ++ [nextCoord, skippedCoord]
                else visitedC ++ [nextCoord]

        newPath = currentP ++ [nextCoord]
    in
    WizardState energyAfterMoveCost newPath newVisited

-- | Función principal recursiva para encontrar todos los caminos válidos.
-- Utiliza recursión para explorar el espacio de estados.
findPaths :: Forest -> WizardState -> [Path]
findPaths forest (WizardState currentEnergy currentPath visitedCells)
    | currentEnergy < 0 = [] -- Camino inválido si la energía cae por debajo de 0 
    | currentCoordinate == (targetRow, targetCol) = [currentPath] -- Caso base: llegó al destino 
    | otherwise = concatMap (findPaths forest) nextWizardStates
    where
        currentCoordinate = last currentPath
        targetRow = length forest - 1
        targetCol = length (head forest) - 1

        -- Genera posibles próximos movimientos
        possibleNextMoves = generatePossibleMoves forest currentCoordinate visitedCells

        -- Crea nuevos estados de mago para cada movimiento válido.
        nextWizardStates = map (applyMove forest (WizardState currentEnergy currentPath visitedCells)) possibleNextMoves

-- | Calcula la energía final de un camino dado.
-- Es una función auxiliar pura para 'findBestPath'.
calculateFinalEnergy :: Forest -> Path -> Int
calculateFinalEnergy forest path =
    let
        -- Para calcular la energía final, simulamos el camino.
        -- foldl es una función de orden superior para reducir una lista.
        -- Partimos del estado inicial (energía inicial y primera celda), y luego aplicamos los movimientos.
        initialStateForPathCalc = WizardState initialEnergy [head path] [head path]
        finalState = foldl (applyMove forest) initialStateForPathCalc (tail path) -- Procesamos el resto del camino
    in
    wsCurrentEnergy finalState

-- | Encuentra el camino con la máxima energía final.
findBestPath :: Forest -> [Path] -> (Path, Int)
findBestPath forest paths =
    -- Filtra los caminos con energía final válida (aunque findPaths ya debería haberlo hecho)
    -- Y luego selecciona el mejor.
    let validPathsWithEnergy = [(path, calculateFinalEnergy forest path) | path <- paths]
        -- maximumBy es una función de orden superior que encuentra el máximo según una función de comparación.
    in maximumBy (comparing snd) validPathWithEnergy

-- 5. Función Principal (Main):
-- Punto de entrada del programa. Contiene la lógica de inicialización y salida.
main :: IO ()
main = do
    -- La matriz del bosque proporcionada en el enunciado 
    let forest = [[ "2", "-3",  "1",  "0",  "2",  "3"],
                  ["-5",  "4", "-2",  "1",  "0", "-4"],
                  [ "1",  "3",  "0", "-3",  "2",  "2"],
                  [ "2", "-1",  "4",  "0", "-5",  "1"],
                  [ "0",  "2", "D",  "3",  "4",  "1"],
                  [ "1",  "0",  "2", "-2",  "1",  "5"]]

    -- Coordenada de inicio 
    let startCoord = (0, 0)
    -- Estado inicial del mago
    let initialWizardState = WizardState initialEnergy [startCoord] [startCoord]

    putStrLn "Buscando el mejor camino en el bosque de runas..."

    -- Encontrar todos los caminos válidos (utilizando la función recursiva)
    let allValidPaths = findPaths forest initialWizardState

    -- Si no se encontraron caminos válidos
    if null allValidPaths
        then putStrLn "No se encontró ningún camino válido."
        else do
            -- Encontrar el mejor camino entre los válidos 
            let (bestPath, finalEnergy) = findBestPath forest allValidPaths

            putStrLn "\n--- Resultado Final ---"
            putStrLn "Mejor Camino Encontrado:"
            print bestPath -- Imprime la lista de coordenadas 
            putStrLn $ "Energía Final: " ++ show finalEnergy -- Imprime la energía final
