--Ejercicio 1 sumar segun-Practica de listas
--Definir la función sumarSegun, que dada una lista de numeros
--y una función devuelve la suma de aplicar la función a cada uno de los elementos.

 --sumarSegun id [1,3,5]
--9
 --sumarSegun length ["hola", "todo", "el", "mundo"]
--15

sumarSegun ::  (a->Int)->[a]->Int
sumarSegun funct lista = foldl1 (+) (map funct lista) 

--Ejercicio 2: exists
--Definir la función exists y declarar su tipo, tal que dadas una función 
--booleana y una lista nos dice si algun elemento de la lista cumple la condición . Ejemplo:

--Main> exists even [1,3,5]
--False

--Main> exists even [1,4,7]
--True -- porque even 4 da True
--Nota: resolverlo utilizando funciones de orden superior y en estilo point free.

exists:: (a->Bool)->[a]->Bool
exists _ []=False
exists funct lista = foldl1 (||) (map funct lista)


--Ejercicio 3: esCapicua
--Definir la función esCapicua/1, que dada una lista de listas,
-- me devuelve si la concatenación de las sublistas es una lista capicua.

--Main> esCapicua ["ne","uqu","en"]
--True

esCapicua lista = concat lista  == reverse (concat lista)

--Ejercicio 4: esMultiploDeAlguno
--Definir la función esMultiploDeAlguno/2, que recibe un número y una lista y devuelve True si el número es múltiplo de alguno de los números de la lista. P.ej.

--Main> esMultiploDeAlguno 15 [2,3,4] 
--True, 
--porque 15 es múltiplo de 3

--Main> esMultiploDeAlguno 34 [3,4,5] 
--False 
--porque 34 no es múltiplo de ninguno de los 3

--Nota: ya se dispone de una función esMultiploDe/2 que puede ser usada en la definición.

esMultiploDeAlguno num lista = any (esMultiploDe num) lista

--Ejercicio 5: Llamadas
--En la compañía telefónica FunTel modelan la histora de llamadas del usuario mediante un par ([Int],[Int]):

--la primera componente es la lista de duraciones de llamadas (en minutos) en el horario normal
--la segunda componente es la lista de duraciones de llamadas (en minutos) en el horario reducido
--Y nos pidieron que obtengamos algunas estadísticas:

--Desarrollá la función cuandoHabloMas, que devuelve el momento en que el usuario habló más cantidad de minutos:

--cuandoHabloMas ([1, 1, 3, 5, 1], [32,20,5])
--"reducido"

--cuandoHabloMas ([1, 10, 10], [2])
--"normal"
--Si en los dos habló la misma cantidad de minutos, da lo mismo responder "reducido" o "normal"

esMayor a b = sumatoria a > sumatoria b 
sumatoria lista = foldl1 (+) lista
cuandoHabloMas listas  |esMayor (fst listas) (snd listas) = "normal"
                       |otherwise = "reducido"

--Ejercicio 6: Más llamadas
--Ah, no tan rápido, tenemos más requerimientos, similares al anterior. Queremos saber:

--cuandoHizoMasLlamadas: en cual de los dos horarios tuvo mayor cantidad de llamadas
--cuandoHizoLaLlamadaMasLarga: en cual de los dos horarios hizo la llamada de mayor duración
--cuandoHizoMasLlamadasBreves: en cual de los dos horarios hizo más llamadas de menos de dos minutos
--cuandoHabloMas: nuestro requerimiento anterior, por supuesto
--Escribí las cuatro funciones anteriores
--Pará, pará, ¿en serio tengo que escribir las cuatro? ¡Si son casi iguales! Es simplemente repetir la misma lógica que hice en el problema anterior 4 veces.

--Aham... escribí las cuatro funciones anteriores, sin repetir lógica entre las cuatro, claro :wink:

esMayor a b = sum a > sum b 

cuando listas function  |function (fst listas) (snd listas) = "normal"
                        |otherwise = "reducido"
mayorCant a b = length a > length b
cuandoHizoMasLlamadas listas = cuando listas mayorCant

llamadaMasLarga a b = eLMasLargo a > eLMasLargo b

eLMasLargo [] = 0	
eLMasLargo (cab:cola) | cab > eLMasLargo cola = cab
                      |otherwise = eLMasLargo cola 

cuandoHizoLaLlamadaMasLarga listas = cuando listas llamadaMasLarga

cuandoHizoMasLlamadasBreves listas = cuando listas mayorCantBreves
mayorCantBreves a b = length (mayoresAdosMin a) > length (mayoresAdosMin b)
mayoresAdosMin lista = filter (<2) lista

cuandoHabloMas listas = cuando listas esMayor

--Ejercicio 7: promedios
--Definir la función promedios/1, que dada una lista de listas me devuelve la lista de los promedios de cada lista-elemento. P.ej.

--Main> promedios [[8,6],[7,9,4],[6,2,4],[9,6]] 
--[7,6.67,4,7.5] 
--Nota: Asumir que se dispone de una función average que devuelve el promedio de una lista

promedios lista= map average lista

--Ejercicio 8: promediosSinAplazos
--Definir la función promediosSinAplazos/1, que dada una lista de listas de notas me devuelve la lista de los promedios de cada lista de notas que contiene, excluyendo para el cálculo del promedio las notas que sean menores a 4 que deberían ignorarse. P.ej.

--Main> promediosSinAplazos [[8,6],[6,2,4]]
--[7,5]
--Nota: Asumir que se dispone de una función average que devuelve el promedio de una lista, y utilizarla.

promediosSinAplazos = map (average.aprobados)
aprobados notas = filter (>=4) notas 

--Ejercicio 9: mejoresNotas
--Definir la función mejoresNotas, que dada la información de un curso, devuelve la lista con la mejor nota de cada alumno. Ejemplo:

--Main> mejoresNotas [[8,6,2,4],[7,9,4,5],[6,2,4,2],[9,6,7,10]]
--[8,9,6,10]
--Nota: Resolverlo utilizando la función del Prelude maximum y alguna función de orden superior :wink:

mejoresNotas = map maximum 

--Ejercicio 10: aprobo
--Definir la función aprobó, que dada la lista de las notas de un alumno devuelve si el alumno aprobó. Se dice que un alumno aprobó si todas sus notas son 4 o más. Ejemplo:

--Main> aprobo [8,6,2,4]
--False

--Main> aprobo [7,9,4,5]
--True
--Nota: resolverlo utilizando funciones de orden superior

aprobo lista = all (>=4) lista 

--Ejercicio 11: aprobaron
--Definir la función aprobaron, que dada la información de un curso devuelve la información de los alumnos que aprobaron. P.ej.

--Main> aprobaron [[8,6,2,4],[7,9,4,5],[6,2,4,2],[9,6,7,10]]
--[[7,9,4,5],[9,6,7,10]]
--Nota: asumir que existe la función aprobo que dice si un alumo (representado mediante una lista de notas) aprobó, y usarla.

aprobaron listas = filter aprobo listas