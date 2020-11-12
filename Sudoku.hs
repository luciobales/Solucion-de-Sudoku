
menu :: IO()
menu = do putStrLn "\n--- --- --- --- --- --- Resolvedor de Sudoku --- --- --- --- --- ---\n"
          putStrLn "Ingrese a continuacion en orden los numeros que figuran en el sudoku"
          putStr "Numero: "
          opc <- getLine 
          let num = read opc :: Int
          putStr "Posicion: "
          opc <- getLine 
          let pos = read opc :: Int
          let lista1 = ingresarNumeros num pos lista
          putStr "Desea ingresar otro numero? (s/n):  "
          rta <-getLine
          if rta == "s"
           then menu2 lista1
           else putStrLn (show lista1) --"Comenzemos!"

menu2 :: [(Int,Int)] -> IO()
menu2 lista1 = do putStr "Numero: "
                  opc <- getLine
                  let num = read opc :: Int
                  putStr "Posicion: "
                  opc <- getLine 
                  let pos = read opc :: Int
                  let lista2 = ingresarNumeros num pos lista1
                  putStr "Desea ingresar otro numero? (s/n):  "
                  rta <-getLine 
                  if rta == "s"
                   then menu2 lista2
                   else putStrLn (show lista2) --"Comenzemos!"

lista = []

-- Funcion que añade numeros ingresados a la lista
ingresarNumeros :: Int -> Int -> [(Int,Int)] -> [(Int,Int)]
ingresarNumeros a b [] = [(a,b)]
ingresarNumeros a b (y:ys) = y:ingresarNumeros a b ys