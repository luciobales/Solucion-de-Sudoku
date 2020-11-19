lista    = []
filas    = 3
columnas = 3
espacios = filas * columnas
matrizVacia = crearMatriz filas columnas

sudoku1 = insertarEnMatriz [(2,1,1)] matrizVacia
sudoku2 = insertarEnMatriz [(5,1,2)] sudoku1
sudoku3 = insertarEnMatriz [(6,2,2)] sudoku2
sudoku4 = insertarEnMatriz [(8,2,3)] sudoku3
sudoku5 = insertarEnMatriz [(1,3,1)] sudoku4
sudoku6 = insertarEnMatriz [(4,3,2)] sudoku5
sudoku  = sudoku6

menu :: IO()
menu = do putStrLn "\n--- --- --- --- --- --- Resolvedor de Sudoku --- --- --- --- --- ---\n"
          putStrLn "Ingrese a continuacion en orden los numeros que figuran en el sudoku"
          putStr "Numero: "
          opc <- getLine 
          let num = read opc :: Integer
          putStr "Fila: "
          opc <- getLine 
          let fila = read opc :: Integer
          putStr "Columna: "
          opc <- getLine 
          let columna = read opc :: Integer
          --let lista1 = ingresarNumeros num fila columna lista
          let lista1 = insertarEnMatriz [(num,fila,columna)] matrizVacia
          putStr "Desea ingresar otro numero? (s/n):  "
          rta <-getLine
          if rta == "s"
           then menu2 lista1
           else menu3 lista1--putStrLn (show lista1)--menu3 lista1 --"Comenzemos!"

menu2 :: [(Integer,Integer,Integer)] -> IO()
menu2 lista1 = do putStr "Numero: "
                  opc <- getLine
                  let num = read opc :: Integer
                  putStr "Fila: "
                  opc <- getLine 
                  let fila = read opc :: Integer
                  putStr "Columna: "
                  opc <- getLine 
                  let columna = read opc :: Integer
                  --let lista2 = ingresarNumeros num fila columna lista1
                  let lista2 = insertarEnMatriz [(num,fila,columna)] lista1
                  putStr "Desea ingresar otro numero? (s/n):  "
                  rta <-getLine 
                  if rta == "s"
                   then menu2 lista2
                   else menu3 lista2--putStrLn (show lista2)--menu3 lista2 --"Comenzemos!"

menu3 :: [(Integer,Integer,Integer)] -> IO() 
menu3 lista2 = do putStr "\n--- --- --- --- --- --- Tablero a resolver --- --- --- --- --- ---\n"
                  mostrarMatriz lista2 
                  putStrLn "Presione cualquier tecla para continuar"
                  rta <-getLine
                  putStrLn "El resultado es... "
                  
-- Ver de crear un menu3 para mostrar sudoku y otras opcs

-- Funcion que añade numeros ingresados del menu a la lista
ingresarNumeros :: Integer -> Integer -> Integer -> [(Integer,Integer,Integer)] -> [(Integer,Integer,Integer)]
ingresarNumeros num fila colu []     = [(num, fila, colu)]
ingresarNumeros num fila colu (y:ys) = y:ingresarNumeros num fila colu ys --Si ingreso 2 veces la misma posicion se rompe


-- Funcion que crea una matriz vacia
crearMatriz :: Integer -> Integer -> [(Integer,Integer,Integer)]
crearMatriz 1 1 = [(0,1,1)]
crearMatriz fila columna = if columna == 1
                            then (crearMatriz (fila-1) (columna+2)) ++ [(0,fila,columna)]
                            else (crearMatriz fila (columna-1)) ++ [(0,fila,columna)]


-- -- Funcion que inserta numeros de la lista a la matriz
insertarEnMatriz :: [(Integer,Integer,Integer)] -> [(Integer,Integer,Integer)] -> [(Integer,Integer,Integer)]
insertarEnMatriz [(num, fila, colu)] [] = [(num, fila, colu)]
insertarEnMatriz [(num, fila, colu)] ((x,y,z):xs) = if fila == y
                                                      then if colu == z
                                                            then (num, fila, colu) : xs
                                                            else (x,y,z) : insertarEnMatriz [(num, fila, colu)] xs
                                                      else (x,y,z) : insertarEnMatriz [(num, fila, colu)] xs


-- Funcion que imprime por pantalla la matriz
mostrarMatriz :: [(Integer,Integer,Integer)] -> IO()
mostrarMatriz [(x,3,3)] = putStrLn (" " ++ show x)
mostrarMatriz ((x,y,z):xs) = do if z == 3
                                 then putStr (" " ++ show x ++ "\n")
                                 else putStr (" " ++ show x ++ " ")
                                mostrarMatriz (xs)
