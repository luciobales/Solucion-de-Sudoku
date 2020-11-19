lista    = []
filas    = 9
columnas = 9
espacios = filas * columnas
matrizVacia = crearMatriz filas columnas


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
                            then (crearMatriz (fila-1) (columna+8)) ++ [(0,fila,columna)]
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
mostrarMatriz [(x,9,9)] = putStrLn (" " ++ show x)
mostrarMatriz ((x,y,z):xs) = do if z == 9
                                 then putStr (" " ++ show x ++ "\n")
                                 else putStr (" " ++ show x ++ " ")
                                mostrarMatriz (xs)




--Ejemplo de sudoku

--sudoku11 = insertarEnMatriz [(0,1,1)] matrizVacia
sudoku12 = insertarEnMatriz [(5,1,2)] matrizVacia
sudoku13 = insertarEnMatriz [(8,1,3)] sudoku12
--sudoku14 = insertarEnMatriz [(0,1,4)] sudoku13
sudoku15 = insertarEnMatriz [(6,1,5)] sudoku13
--sudoku16 = insertarEnMatriz [(0,1,6)] sudoku15
sudoku17 = insertarEnMatriz [(7,1,7)] sudoku15
sudoku18 = insertarEnMatriz [(1,1,8)] sudoku17
sudoku19 = insertarEnMatriz [(9,1,9)] sudoku18

sudoku21 = insertarEnMatriz [(4,2,1)] sudoku19
sudoku22 = insertarEnMatriz [(9,2,2)] sudoku21
sudoku23 = insertarEnMatriz [(7,2,3)] sudoku22
--sudoku24 = insertarEnMatriz [(0,2,4)] sudoku23
--sudoku25 = insertarEnMatriz [(0,2,5)] sudoku24
sudoku26 = insertarEnMatriz [(8,2,6)] sudoku23
sudoku27 = insertarEnMatriz [(6,2,7)] sudoku26
sudoku28 = insertarEnMatriz [(3,2,8)] sudoku27
sudoku29 = insertarEnMatriz [(5,2,9)] sudoku28

sudoku31 = insertarEnMatriz [(1,3,1)] sudoku29
sudoku32 = insertarEnMatriz [(6,3,2)] sudoku31
sudoku33 = insertarEnMatriz [(3,3,3)] sudoku32
sudoku34 = insertarEnMatriz [(9,3,4)] sudoku33
sudoku35 = insertarEnMatriz [(7,3,5)] sudoku34
sudoku36 = insertarEnMatriz [(5,3,6)] sudoku35
sudoku37 = insertarEnMatriz [(2,3,7)] sudoku36
sudoku38 = insertarEnMatriz [(4,3,8)] sudoku37
sudoku39 = insertarEnMatriz [(8,3,9)] sudoku38

--sudoku41 = insertarEnMatriz [(0,4,1)] sudoku39
sudoku42 = insertarEnMatriz [(8,4,2)] sudoku39
sudoku43 = insertarEnMatriz [(6,4,3)] sudoku42
sudoku44 = insertarEnMatriz [(2,4,4)] sudoku43
sudoku45 = insertarEnMatriz [(1,4,5)] sudoku44
sudoku46 = insertarEnMatriz [(7,4,6)] sudoku45
sudoku47 = insertarEnMatriz [(3,4,7)] sudoku46
sudoku48 = insertarEnMatriz [(5,4,8)] sudoku47
sudoku49 = insertarEnMatriz [(4,4,9)] sudoku48

sudoku51 = insertarEnMatriz [(3,5,1)] sudoku49
sudoku52 = insertarEnMatriz [(7,5,2)] sudoku51
sudoku53 = insertarEnMatriz [(4,5,3)] sudoku52
sudoku54 = insertarEnMatriz [(5,5,4)] sudoku53
sudoku55 = insertarEnMatriz [(9,5,5)] sudoku54
sudoku56 = insertarEnMatriz [(6,5,6)] sudoku55
sudoku57 = insertarEnMatriz [(8,5,7)] sudoku56
sudoku58 = insertarEnMatriz [(2,5,8)] sudoku57
sudoku59 = insertarEnMatriz [(1,5,9)] sudoku58

sudoku61 = insertarEnMatriz [(5,6,1)] sudoku59
sudoku62 = insertarEnMatriz [(2,6,2)] sudoku61
--sudoku63 = insertarEnMatriz [(0,6,3)] sudoku62
sudoku64 = insertarEnMatriz [(8,6,4)] sudoku62
sudoku65 = insertarEnMatriz [(3,6,5)] sudoku64
--sudoku66 = insertarEnMatriz [(0,6,6)] sudoku65
sudoku67 = insertarEnMatriz [(9,6,7)] sudoku65
sudoku68 = insertarEnMatriz [(7,6,8)] sudoku67
sudoku69 = insertarEnMatriz [(6,6,9)] sudoku68

sudoku71 = insertarEnMatriz [(8,7,1)] sudoku69
sudoku72 = insertarEnMatriz [(3,7,2)] sudoku71
sudoku73 = insertarEnMatriz [(9,7,3)] sudoku72
sudoku74 = insertarEnMatriz [(7,7,4)] sudoku73
sudoku75 = insertarEnMatriz [(4,7,5)] sudoku74
sudoku76 = insertarEnMatriz [(1,7,6)] sudoku75
sudoku77 = insertarEnMatriz [(5,7,7)] sudoku76
sudoku78 = insertarEnMatriz [(6,7,8)] sudoku77
sudoku79 = insertarEnMatriz [(2,7,9)] sudoku78

sudoku81 = insertarEnMatriz [(6,8,1)] sudoku79
sudoku82 = insertarEnMatriz [(4,8,2)] sudoku81
sudoku83 = insertarEnMatriz [(5,8,3)] sudoku82
sudoku84 = insertarEnMatriz [(3,8,4)] sudoku83
sudoku85 = insertarEnMatriz [(8,8,5)] sudoku84
sudoku86 = insertarEnMatriz [(2,8,6)] sudoku85
--sudoku87 = insertarEnMatriz [(0,8,7)] sudoku86
sudoku88 = insertarEnMatriz [(9,8,8)] sudoku86
sudoku89 = insertarEnMatriz [(7,8,9)] sudoku88

sudoku91 = insertarEnMatriz [(7,9,1)] sudoku89
sudoku92 = insertarEnMatriz [(1,9,2)] sudoku91
sudoku93 = insertarEnMatriz [(2,9,3)] sudoku92
sudoku94 = insertarEnMatriz [(6,9,4)] sudoku93
sudoku95 = insertarEnMatriz [(5,9,5)] sudoku94
sudoku96 = insertarEnMatriz [(9,9,6)] sudoku95
sudoku97 = insertarEnMatriz [(4,9,7)] sudoku96
sudoku98 = insertarEnMatriz [(8,9,8)] sudoku97
sudoku99 = insertarEnMatriz [(3,9,9)] sudoku98

sudoku  = sudoku99
