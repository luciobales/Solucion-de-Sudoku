lista = []
fin = 80
inicio = 0
matrizVacia = crearMatriz inicio fin

menu :: IO()
menu = do putStrLn "\n--- --- --- --- --- --- Resolvedor de Sudoku --- --- --- --- --- ---\n"
          putStrLn "Ingrese a continuacion en orden los numeros que figuran en el sudoku"
          putStr "Numero: "
          opc <- getLine 
          let num = read opc :: Integer
          putStr "Posicion: "
          opc <- getLine 
          let pos = read opc :: Integer
          let lista1 = ingresarNumeros num pos lista
          putStr "Desea ingresar otro numero? (s/n):  "
          rta <-getLine
          if rta == "s"
           then menu2 lista1
           else putStrLn (show lista1) --"Comenzemos!"

menu2 :: [(Integer,Integer)] -> IO()
menu2 lista1 = do putStr "Numero: "
                  opc <- getLine
                  let num = read opc :: Integer
                  putStr "Posicion: "
                  opc <- getLine 
                  let pos = read opc :: Integer
                  let lista2 = ingresarNumeros num pos lista1
                  putStr "Desea ingresar otro numero? (s/n):  "
                  rta <-getLine 
                  if rta == "s"
                   then menu2 lista2
                   else putStrLn (show lista2) --"Comenzemos!"

-- Funcion que añade numeros ingresados a la lista
ingresarNumeros :: Integer -> Integer -> [(Integer,Integer)] -> [(Integer,Integer)]
ingresarNumeros a b [] = [(a,b)]
ingresarNumeros a b (y:ys) = y:ingresarNumeros a b ys

crearMatriz :: Integer -> Integer -> [(Integer,Integer)]
crearMatriz inicio 0 = [(0,fin)]
crearMatriz inicio fin = (0,inicio) : (crearMatriz (inicio+1) (fin-1))

insertarEnMatriz :: [(Integer,Integer)] -> [(Integer,Integer)] -> [(Integer,Integer)]
insertarEnMatriz [(a,b)] ((x,y):xs) = if b == y
                                       then (a,b):xs
                                       else (x,y) : insertarEnMatriz [(a,b)] xs





