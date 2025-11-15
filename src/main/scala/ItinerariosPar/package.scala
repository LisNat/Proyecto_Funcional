import Datos._
import common._
import Itinerarios._

import scala.collection.parallel.CollectionConverters._
import scala.collection.parallel.ParSeq

package object ItinerariosPar {

  def itinerariosPar(vuelos: List[Vuelo], aeropuertos: List[Aeropuerto]): (String, String) => List[Itinerario] = {
    // Recibe vuelos, una lista de todos los vuelos disponibles y
    // aeropuertos una lista de todos los aeropuertos
    // y devuelve una funcion que recibe c1 y c2, codigos de aeropuertos
    // y devuelve todos los itinerarios entre esos dos aeropuertos
    ???
  }

  def itinerariosTiempoPar(vuelos: List[Vuelo], aeropuertos: List[Aeropuerto]): (String, String) => List[Itinerario] = {
    // recibe c1 y c2, codigos de aeropuertos
    // y devuelve los tres (si los hay) itinerarios que minimizan el tiempo de viaje entre esos dos aeropuertos
    ???
  }

  def itinerariosEscalasPar(vuelos: List[Vuelo], aeropuertos: List[Aeropuerto]): (String, String) => List[Itinerario] = {
    def calcularEscalasPar(itinerario: Itinerario): Int = {
      // 1. Suma las escalas técnicas informadas en cada vuelo
      val escalasTecnicas = itinerario.map(_.Esc).sum

      // 2. Suma las escalas de conexión (un itinerario de N vuelos tiene N-1 conexiones)
      val escalasPorConexion = if (itinerario.isEmpty) 0 else itinerario.length - 1

      escalasTecnicas + escalasPorConexion
    }

    (cod1: String, cod2: String) => {
      // 1. Inicia la búsqueda para encontrar todos los itinerarios
      val todosLosItinerarios = itinerariosPar(vuelos, aeropuertos)(cod1, cod2)

      // 2. Ordena todos los itinerarios encontrados usando la función 'calcularEscalas'
      val itinerariosOrdenados = todosLosItinerarios.sortBy(calcularEscalasPar)

      // 3. Devuelve los 3 mejores (con menos escalas)
      itinerariosOrdenados.take(3)
    }
  }

  def itinerariosAirePar(vuelos: List[Vuelo], aeropuertos: List[Aeropuerto]): (String, String) => List[Itinerario] = {
    // recibe c1 y c2, codigos de aeropuertos
    // y devuelve los tres (si los hay) itinerarios que minimizan el tiempo en el aire entre esos dos aeropuertos
    ???
  }

  def itinerarioSalidaPar(vuelos: List[Vuelo], aeropuertos: List[Aeropuerto]): (String, String, Int, Int) => Itinerario = {
    // recibe c1 y c2, codigos de aeropuertos
    // y devuelve el itinerario que optimiza la hora de salida para llegar a tiempo a la cita
    ???
  }

}
