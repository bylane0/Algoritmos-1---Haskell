-- GRUPO: or1=1--

-- INTEGRANTES: 
-- Integrante 1: Aubone Juan Pablo | LU: 336/23 | email: juanaubone1234@gmail.com
-- Integrante 2: Palacín Roitbarg Tobías | LU: 6/23 | email: tobiaspalacinroitbarg@gmail.com
-- Integrante 3: D`Andrea Matias | LU: 1049/22 | email: mdandrea03@gmail.com
-- Integrante 4: 

---------------DEFINICIONES-----------------------------------------------------------------------------------------------------
--1
type Usuario = (Integer, String) -- (id, nombre)
--2
type Relacion = (Usuario, Usuario) -- usuarios que se relacionan
--3
type Publicacion = (Usuario, String, [Usuario]) -- (usuario que publica, texto publicacion, likes)
--4
type RedSocial = ([Usuario], [Relacion], [Publicacion])
--------------------------------------------------------------------------------------------------------------------------------

---------------FUNCIONES BASICAS------------------------------------------------------------------------------------------------
--1
usuarios :: RedSocial -> [Usuario]
usuarios red = fst3 red  

fst3 :: (a, b, c) -> a
fst3 (a,_,_) = a
-------------
--2
relaciones :: RedSocial -> [Relacion]
relaciones red = snd3 red

snd3 :: (a, b, c) -> b
snd3 (_,b,_) = b
-------------
--3
publicaciones :: RedSocial -> [Publicacion]
publicaciones red = thrd3 red

thrd3 :: (a, b, c) -> c
thrd3 (_,_,c) = c
-------------
--4
idDeUsuario :: Usuario -> Integer
idDeUsuario u = fst2 u

fst2 :: (a,b) -> a
fst2 (a,_) = a
-------------
--5
nombreDeUsuario :: Usuario -> [Char]
nombreDeUsuario u = snd2 u

snd2 :: (a,b) -> b
snd2 (_,b) = b
-------------
--6
usuarioDePublicacion :: Publicacion -> Usuario
usuarioDePublicacion p = fst3 p 
-------------
--7
likesDePublicacion :: Publicacion -> [Usuario]
likesDePublicacion p = thrd3 p
--------------------------------------------------------------------------------------------------------------------------------

---------------PREDICADOS AUXILIARES--------------------------------------------------------------------------------------------
--1
pertenece :: (Eq t) => t -> [t] -> Bool
pertenece _ [] = False
pertenece n (x:xs) = elem n (x:xs)
-------------
--2
mismosElementos :: (Eq t) => [t] -> [t] -> Bool
mismosElementos [] [] = True
mismosElementos [] (x:xs) = False 
mismosElementos [x] [y] = x==y
mismosElementos (x:xs) (y:ys) | not (pertenece x  (y:ys)) = False
                              | otherwise = mismosElementos (quitarTodas x (x:xs)) (quitarTodas x (y:ys))
 
quitarTodas :: (Eq t) => t -> [t] -> [t]
quitarTodas _ [] = []
quitarTodas y (x:xs) | x == y = quitarTodas y xs
              | otherwise = x : quitarTodas y xs
-------------
--3 RED SOCIAL VALIDA (NECESITA --4 ; --6 ; --7)
redSocialValida :: RedSocial -> Bool
redSocialValida red | ((usuariosValidos (usuarios red)) && (relacionesValidas (usuarios red) (relaciones red)) && (publicacionesValidas (usuarios red) (publicaciones red))) = True
                    | otherwise = False
-------------
--4 USUARIOS VALIDOS               COMPROBADO!!!!
usuariosValidos :: [Usuario] -> Bool
usuariosValidos usu | (((usuariosValidosAUX usu ((length usu)-1)) == True) && ((noHayIdsRepetidos usu) == True)) = True 
                    | otherwise = False

usuariosValidosAUX :: [Usuario] -> Int -> Bool
usuariosValidosAUX us 0 = usuarioValido (us !! 0) 
usuariosValidosAUX us x | (usuarioValido (us !! x) == True) = usuariosValidosAUX us (x-1)
                        | otherwise = False

usuarioValido :: Usuario -> Bool
usuarioValido u | (((idDeUsuario u) > 0) && ((length (nombreDeUsuario u)) > 0)) = True
                | otherwise = False

noHayIdsRepetidos :: [Usuario] -> Bool
noHayIdsRepetidos [x] = True
noHayIdsRepetidos (x:xs) | ((fst2 x) == (fst2 (head xs))) = False 
                         | otherwise = noHayIdsRepetidos xs
-------------
--5
--IDEA: Usar para la funcion  ¿¿¿¨noHayRelacionesRepetidas¨???
todosDistintos :: (Eq t) => [t] -> Bool
todosDistintos [] = True
todosDistintos [x] = True
todosDistintos (x:xs) = not (pertenece x xs) && todosDistintos xs
-------------
--6 RELACIONES VALIDAS
relacionesValidas :: [Usuario] -> [Relacion] -> Bool  --comprueba que todos los usuarios que estan en las duplas de las relaciones pertenescan a la lista de los usuarios
relacionesValidas us rels | ((usuariosDeRelacionValidos us rels == True) && (relacionesAsimetricas rels == True) && (noHayRelacionesRepetidas rels == True)) = True
                          | otherwise = False

--primera condicion de RELACIONES VALIDAS----------------                        
usuariosDeRelacionValidos :: [Usuario] -> [Relacion] -> Bool 
usuariosDeRelacionValidos us rels | ((usuariosDeRelacionValidosAUX3 rels us == True) && (usuariosDeRelacionValidosAUX2 rels us == True) && ((usuariosDeRelacionValidosAUX1 rels ((length rels)-1)) == True)) = True
                                  | otherwise = False

-- rels[x]0 ̸= rels[x]1 :
usuariosDeRelacionValidosAUX1 :: [Relacion] -> Int -> Bool -- posible cambio de recursividad para ser capo (juan forro)
usuariosDeRelacionValidosAUX1 rel 0 | (fst2 (rel !! 0)) /= (snd2 ((rel !! 0))) = True
                                    | otherwise = False
usuariosDeRelacionValidosAUX1 rel n | ((fst2 (rel !! n)) /= (snd2 ((rel !! n)))) = usuariosDeRelacionValidosAUX1 rel (n-1)
                                    | otherwise = False

-- Pertenece(rels[x]0, us) :
usuariosDeRelacionValidosAUX2 :: [Relacion] -> [Usuario] -> Bool                                  
usuariosDeRelacionValidosAUX2 [x] u = pertenece (fst2 x) u 
usuariosDeRelacionValidosAUX2 (x:xs) u | ((pertenece (fst2 x) u) == True) = usuariosDeRelacionValidosAUX2 xs u
                                       | otherwise = False
-- Pertenece(rels[x]1, us) :
usuariosDeRelacionValidosAUX3 :: [Relacion] -> [Usuario] -> Bool                                  
usuariosDeRelacionValidosAUX3 [x] u = pertenece (snd2 x) u 
usuariosDeRelacionValidosAUX3 (x:xs) u | ((pertenece (snd2 x) u) == True) = usuariosDeRelacionValidosAUX3 xs u
                                       | otherwise = False
-----------------------------------------------------------

-- segunda condicion de RELACIONES VALIDAS-----------------
relacionesAsimetricas :: [Relacion] -> Bool
relacionesAsimetricas rels = not (relacionesAsimetricasAUX rels ((length rels) - 1))

relacionesAsimetricasAUX :: [Relacion] -> Int -> Bool
relacionesAsimetricasAUX rels 0 | ((pertenece (snd2 (rels !! 0), fst2 (rels !! 0)) rels) == True) = True
                                | otherwise = False
relacionesAsimetricasAUX rels n | ((pertenece (snd2 (rels !! n), fst2 (rels !! n)) rels) == True) = relacionesAsimetricasAUX rels (n-1)
                                | otherwise = False
                        
-----------------------------------------------------------
--tercera condicion de RELACIONES VALIDAS   
noHayRelacionesRepetidas :: [Relacion] -> Bool
noHayRelacionesRepetidas [x] = True
noHayRelacionesRepetidas (x:xs) | (noHayRelacionesRepetidasAUX xs x == True) = noHayRelacionesRepetidas xs 
                                | otherwise = False

noHayRelacionesRepetidasAUX :: [Relacion] -> Relacion -> Bool
noHayRelacionesRepetidasAUX [x] rel = ( ((idDeUsuario (fst2 x)) /= (idDeUsuario (fst2 (rel)))) || ((idDeUsuario (snd2 x)) /= (idDeUsuario(snd2 rel))) )  
noHayRelacionesRepetidasAUX rels rel | (noHayRelacionesRepetidasAUX [(head rels)] rel) == True = noHayRelacionesRepetidasAUX (tail rels) rel
                                     | otherwise = False
-------------
 --7 PUBLICACIONES VALIDAS
publicacionesValidas :: [Usuario] -> [Publicacion] -> Bool
publicacionesValidas us pubs | (((usuariosDePublicacionSonUsuariosDeRed us pubs) == True) &&  ((noHayPublicacionesRepetidas pubs) == True)) = True
                             | otherwise = False

--primera condicion PUBLICACIONES VALIDAS
usuariosDePublicacionSonUsuariosDeRed :: [Usuario] -> [Publicacion] -> Bool
usuariosDePublicacionSonUsuariosDeRed us [x] = pertenece (usuarioDePublicacion x) us
usuariosDePublicacionSonUsuariosDeRed us (x:xs) | ((pertenece (usuarioDePublicacion x) us) == True) = usuariosDePublicacionSonUsuariosDeRed us xs  
                                                | otherwise = False

--segunda condicion PUBLICACIONES VALIDAS
noHayPublicacionesRepetidas :: [Publicacion] -> Bool
noHayPublicacionesRepetidas [x] = True
noHayPublicacionesRepetidas (x:xs) | noHayPublicacionesRepetidasAUX xs x == True = noHayPublicacionesRepetidas xs
                                   | otherwise = False

noHayPublicacionesRepetidasAUX :: [Publicacion] -> Publicacion -> Bool 
noHayPublicacionesRepetidasAUX [x] pub = ( (idDeUsuario (usuarioDePublicacion x)) /= (idDeUsuario (usuarioDePublicacion pub)) ) || snd3 x /= snd3 pub
noHayPublicacionesRepetidasAUX (x:xs) pub | noHayPublicacionesRepetidasAUX (x:xs) pub == True = noHayPublicacionesRepetidasAUX xs pub 
                                          | otherwise = False
   -- ( (idDeUsuario (usuarioDePublicacion (pubs !! 0))) /= (idDeUsuario (usuarioDePublicacion (pubs !! 1))) ) || snd3 (pubs !! 0) /= snd3 (pubs !! 1)
-------------
--8
cadenaDeAmigos :: [Usuario] -> RedSocial -> Bool
cadenaDeAmigos [u1,u2] red = relacionadosDirecto u1 u2 red 
cadenaDeAmigos (x:xs) red | (relacionadosDirecto x (head xs) red == True) = cadenaDeAmigos xs red
                          | otherwise = False

relacionadosDirecto :: Usuario -> Usuario -> RedSocial -> Bool
relacionadosDirecto u1 u2 red | (pertenece (u1,u2) (relaciones red) == True || pertenece (u2,u1) (relaciones red) == True) = True
                              | otherwise = False
-------------
--9
sonDeLaRed :: RedSocial -> [Usuario] -> Bool
sonDeLaRed red [x] = pertenece x (usuarios red)
sonDeLaRed red (x:xs) | pertenece x (usuarios red) = sonDeLaRed red xs
                      | otherwise = False
-------------
--10
empiezaCon :: (Eq t) => t -> [t] -> Bool
empiezaCon e l | (head l == e) = True                  
               | otherwise = False
-------------
--11
terminaCon :: (Eq t) => t -> [t] -> Bool
terminaCon e [a] | a == e = True
                 | otherwise = False
terminaCon e (x:xs) = terminaCon e xs
-------------
--12
sinRepetidos :: (Eq t) => [t] -> Bool
sinRepetidos l = todosDistintos l  -- aca si funciona todosDistintos
-------------
--------------------------------------------------------------------------------------------------------------------------------

---------------EJERCICIOS-------------------------------------------------------------------------------------------------------

--EJERCICIO 1

-- nombresDeUsuarios :: RedSocial -> [String]
-- nombresDeUsuarios a = 