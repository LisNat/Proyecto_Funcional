=============================================================================
PROYECTO: PLANIFICACIÓN DE VUELOS (PROGRAMACIÓN FUNCIONAL Y CONCURRENTE)
=============================================================================

AUTORES:
- Liseth Natalia Rivera (2223510)
- Santiago Vanegas Torres (2416930)
- Jorge Luis Junior Lasprilla (2420662)
- Andres Mauricio Rengifo (1926987)

INSTRUCCIONES DE EJECUCIÓN
El proyecto está configurado para ejecutarse mediante SBT (Scala Build Tool).
El build.sbt se definió de la siguiente manera:

ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "2.13.10"

lazy val root = (project in file("."))
  .settings(
    name := "Proyecto_PFuncional"
  )
scalacOptions ++= Seq("-language:implicitConversions", "-deprecation")
libraryDependencies ++= Seq(
  ("com.storm-enroute" %% "scalameter-core" % "0.21").cross(CrossVersion.for3Use2_13),
  "org.scala-lang.modules" %% "scala-parallel-collections" % "1.0.3",
  "org.scalameta" %% "munit" % "0.7.26" % Test
)
libraryDependencies += "org.scala-lang.modules" %% "scala-parallel-collections" % "1.0.4"


PASOS:
1. Abra una terminal en la carpeta raíz del proyecto.
2. Inicie la consola de sbt: sbt console
3. Una vez dentro de la consola de Scala, ejecute los siguientes comandos
   para importar los módulos y cargar el script de pruebas:
   scala> import common._
   scala> import Datos._
   scala> import Itinerarios._
   scala> import ItinerariosPar._

   scala> :load src\test\scala\Pruebas.sc (Windows)

DESCRIPCIÓN DE FUNCIONES

A. Modelo Secuencial (Itinerarios)

Este archivo define un package object llamado Itinerarios, encargado de generar, ordenar y analizar diferentes rutas de viaje entre aeropuertos usando una colección de vuelos y aeropuertos. Su propósito es calcular todos los itinerarios posibles entre dos códigos de aeropuerto y luego ofrecer funciones especializadas para ordenarlos según criterios como tiempo total, escalas, tiempo en aire o conveniencia de llegada para una cita. Además, incluye herramientas para convertir horas a UTC para comparar correctamente los horarios entre diferentes zonas horarias.

# Explicación sencilla de cada función

1. itinerarios
Genera una función que, dadas dos siglas de aeropuerto, encuentra *todos los itinerarios posibles* desde origen a destino.
Usa una búsqueda recursiva que explora los vuelos disponibles, evitando ciclos, y construye listas de vuelos que representan cada ruta válida.

2. convertirAMinutosUTC
Convierte una hora local (hora y minutos) a minutos en formato UTC usando el GMT del aeropuerto.
Es útil para normalizar horarios entre diferentes zonas horarias.

3. itinerariosTiempo
Devuelve los 3 itinerarios más rápidos entre dos aeropuertos.
Calcula el tiempo total real de un itinerario considerando:
* la hora UTC de salida y llegada,
* esperas entre conexiones,
* cambios de día cuando un vuelo llega al día siguiente.
Luego ordena todos los itinerarios por tiempo total y selecciona los tres mejores.

4. itinerariosEscalas
Devuelve los 3 itinerarios con menos escalas.
Cuenta:
* las escalas técnicas (`Esc` de cada vuelo),
* las escalas por conexión (un itinerario de N vuelos tiene N−1).

Ordena por ese total y devuelve los tres primeros.

5. itinerariosAire
Devuelve los 3 itinerarios con menor tiempo en el aire.
Para cada vuelo calcula su duración real en UTC (corrigiendo si cruza medianoche) y luego suma todo el tiempo en vuelo de un itinerario.
Ordena por ese tiempo total en aire.

6. itinerarioSalida
Devuelve el mejor itinerario para llegar a una cita en un horario dado en el aeropuerto destino.
Ordena todos los itinerarios según:
1. cuántos días antes llegan (0 es ideal),
2. la salida más tardía posible (para no salir demasiado temprano),
3. la duración total como desempate.
Devuelve solo el mejor itinerario.

B. Modelo Paralelo (ItinerariosPar)

Este archivo define un conjunto de funciones paralelas para calcular itinerarios de vuelos entre aeropuertos. Es una versión optimizada del módulo secuencial Itinerarios, pero aquí se aprovecha el paralelismo mediante task y colecciones paralelas (.par) para acelerar tanto la búsqueda de rutas como el cálculo de métricas (tiempo total, escalas, tiempo en aire, criterios de llegada/salida). El objetivo es mejorar el rendimiento cuando existen muchas combinaciones posibles de vuelos.

# Explicación de cada función

1. itinerariosPar
Calcula todos los itinerarios posibles entre dos aeropuertos usando paralelismo.
- Agrupa los vuelos por origen.
- Usa la función recursiva buscarItinerariosPar, que:
 	- Si el origen llega al destino, devuelve un itinerario vacío.
 	- Filtra vuelos no visitados.
 	- Si hay 1 o 0 opciones, sigue secuencial (más rápido).
 	- Si hay varias, crea tareas paralelas (task) para explorar cada vuelo.
- Devuelve la lista completa de itinerarios válidos.

2. itinerariosTiempoPar
Encuentra los 3 itinerarios más rápidos en términos de tiempo total (espera + vuelo), usando paralelismo.
- Calcula el tiempo total de cada itinerario igual que en la versión secuencial.
- Obtiene primero todos los itinerarios de manera paralela con itinerariosPar.
- Si hay pocos itinerarios usa proceso normal; si hay varios paraleliza el cálculo del tiempo de cada itinerario mediante task.
- Ordena por tiempo total y devuelve los 3 mejores.

3. itinerariosEscalasPar
Encuentra los itinerarios con menos escalas, calculando en paralelo.
- Cuenta:
 	- Escalas técnicas (por vuelo).
 	- Escalas por conexión (N-1).
- Obtiene todos los itinerarios con itinerariosPar.
- Para cada uno calcula las escalas en paralelo usando task.
- Ordena por menor número de escalas y devuelve los 3 primeros.

4. itinerariosAirePar
Devuelve los 3 itinerarios con menor tiempo en aire, completamente paralelizado.
- Calcula la duración real de cada vuelo ajustando por GMT.
- tiempoEnAireTotal calcula el tiempo sumando duraciones en paralelo (it.par).
- Los itinerarios se obtienen con itinerariosPar.
- Luego se calcula el tiempo total de cada itinerario usando .par para paralelizar.
Se ordenan y se devuelven los mejores tres.
 
5. itinerarioSalidaPar
Devuelve el mejor itinerario para llegar a una cita, optimizando para una salida lo más tarde posible pero llegando a tiempo.
- Recalcula convertirAMinutosUTC, horaSalida, horaLlegada y tiempoTotal.
- Obtiene los itinerarios desde itinerariosPar.
- Convierte la hora de la cita al valor UTC.
- Para cada itinerario, en paralelo:
 	- Calcula cuántos días antes llega.
 	- Considera la hora de salida (en negativo para ordenar de mayor a menor).
 	- Usa el tiempo total como desempate.
- Ordena por estos criterios y devuelve el itinerario más conveniente.
