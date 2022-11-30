--Practica Final Haskell: Aitor Esteban Núñez

--Definimos el tipo Hash con sus funciones necesarias

type Clave = String
type Valor = String
type HashElem = (Clave, Valor)
type ListaHash = [HashElem]
data HashTable = Hash [ListaHash]

--Mostramos la tabla hash según el formato indicado

showListaHash :: ListaHash -> String
showListaHash [] = "\n"
showListaHash (x:xs) = show x ++ "," ++ showListaHash xs

showHashTable :: HashTable -> Int -> String 
showHashTable (Hash []) _ = "\n"
showHashTable (Hash (x:xs)) n = "|" ++ show n ++ "|->"++showListaHash x ++ showHashTable (Hash xs) (n+1)

instance Show HashTable where
    show (Hash table) = showHashTable (Hash table) 0
    
--Como función hash usamos la evaluación de un polinomio por la regla de Horner y hacemos modulo el numero de posibles valores hash que queramos tener 
funcionHash :: HashElem -> Int
funcionHash ([],_) = 0
funcionHash (c:cs, vs) = mod (fromEnum c + funcionHash (cs, vs)) 10

--Función que dado una par clave valor calcula el índice de la Hashtable en el que tiene que estar (lista según el valor de la función hash) y lo introduce en la tabla
--utilizando las funciones auxiliares separarTablaEnIndice y añadeElemASuLista para modificar la [ListaHash] de forma que tenga el nuevo elemento
añadeHashElem :: HashElem -> HashTable -> HashTable
añadeHashElem (c,v) (Hash l) = let {indice = funcionHash (c,v); nuevaLista = añadeElemASuLista' l indice (c,v)} in Hash nuevaLista  

--Dada la lista con las listas Hash, el índice de la lista hash a la que hay que añadir el elemento y el elemento, separamos la lista de lista por ese punto con splitAt
--y añadimos el elmento donde corresponde
añadeElemASuLista':: [ListaHash] -> Int -> HashElem -> [ListaHash]
añadeElemASuLista' l 0 e = (head l ++ [e]): tail l 
añadeElemASuLista' l 9 e = xs ++ [head ys ++ [e]] where (xs, ys) = splitAt 9 l 
añadeElemASuLista' l indice e = xs ++ [head ys ++ [e]] ++ tail ys where (xs, ys) = splitAt indice l 

--Convierte a (c,v) una lista con dos elementos ["c","v"]
convierteAHashElem:: [String] -> HashElem
convierteAHashElem l = (head l, l !! 1)

--Inicializar una tabla Hash a partir del fichero datos. Leemos el fichero por lineas y creamos una Hashtable a partir de los datos
inicializaAux :: String -> HashTable -> IO HashTable
inicializaAux fileIn tabla = do
    file <- readFile fileIn
    let traducciones = lines file --["palabra traduccion", "", "", ...]
    let listaTraducciones = map words traducciones -- [["palabra","traduccion"], ["",""], ...]
    let listaTraduccionesHashElems = map convierteAHashElem listaTraducciones -- [(palabra, traduccion), (,), ...]
    let tablaInicializada = foldl (flip añadeHashElem) tabla listaTraduccionesHashElems  --usamos foldr en el que usamos tabla como elemento neutro para añadir siempre
    --los elementos a la misma tabla
    return tablaInicializada --print HashTable cuando esté (cambiar tipo a IO ())

--La tabla Hash inicial tiene que tener 9 tablas vacías para cada valor hash 
inicializa::String -> IO HashTable
inicializa fileIn = inicializaAux fileIn (Hash [[],[],[],[],[],[],[],[],[],[]])

--Leemos las palabras a traducir en una lista de Strings. El usuario debe escribir las palabras con un espacio de separación.
leePalabras:: IO [String]
leePalabras = do 
    putStr "Introduce palabras a traducir separadas pur espacio:"
    palabras <- getLine
    let lista = words palabras
    return lista


--Calculo de la longitud media de palabras introducidas. Sumamos la longitud de todas las palabras de la lista dada y lo dividimos por el numero 
--de palabras. Utilizamos la función fromIntegral para convertir Integral a Float y poder dividir
calculaLongitud::  [String] -> Float
calculaLongitud [] = 0
calculaLongitud palabras = fromIntegral (sum  (map length palabras)) / fromIntegral (length palabras)


--Dada una palabra busca su traducción en la lista hash en la que esta
traduceListaHash:: String -> ListaHash -> String
traduceListaHash palabra [] = "La palabra " ++ palabra ++" no esta en el diccionario"
traduceListaHash palabra (x:xs) = if palabra == fst x then snd x else traduceListaHash palabra xs

--Dada una palabra y un diccionario, busca el indice de la lista hash en la que se encuentra la palabra y busca su traduccion usando la funcion traduceListaHash
traducePalabra:: String -> HashTable -> String
traducePalabra palabra (Hash l) = traduceListaHash palabra (l !! indice) where indice = funcionHash (palabra,"")

--Dada una lista de palabras y un diccionario, traduce las palabras
traducePalabras::[String] -> HashTable -> [String]
traducePalabras palabras tabla = [traducePalabra x tabla | x <- palabras]

--Función "main" que inicializa el diccionario dado un fichero, pide palabras al usuario y las traduce
traducir :: String -> IO ()
traducir fileIn = do 
    tabla <- inicializa fileIn
    putStr (show tabla)
    palabras <- leePalabras
    putStr  "Traducciones: "
    print $ traducePalabras palabras tabla
    