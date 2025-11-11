import Datos._
package object Itinerarios {

  def itinerarios(vuelos: List[Vuelo], aeropuertos: List[Aeropuerto]): (String, String) => List[Itinerario] = {
    // Recibe vuelos, una lista de todos los vuelos disponibles y
    // aeropuertos una lista de todos los aeropuertos
    // y devuelve una funcion que recibe c1 y c2, codigos de aeropuertos
    // y devuelve todos los itinerarios entre esos dos aeropuertos
  ???
  }

  def itinerariosTiempo(vuelos: List[Vuelo], aeropuertos: List[Aeropuerto]): (String, String) => List[Itinerario] = {
    // Recibe vuelos, una lista de todos los vuelos disponibles y
    // aeropuertos una lista de todos los aeropuertos
    // y devuelve una funcion que recibe c1 y c2, codigos de aeropuertos
    // y devuelve una funcion que devuelve los tres (si los hay) itinerarios que minimizan el tiempo de viaje entre esos dos aeropuertos
    ???
  }

  def itinerariosEscalas(vuelos: List[Vuelo], aeropuertos: List[Aeropuerto]): (String, String) => List[Itinerario] = {
    // Recibe vuelos, una lista de todos los vuelos disponibles y
    // aeropuertos una lista de todos los aeropuertos
    // y devuelve una funcion que recibe c1 y c2, codigos de aeropuertos
    // y devuelve los tres (si los hay) itinerarios que minimizan el numero de cambios de avion entre esos dos aeropuertos
    ???
  }

  def itinerariosAire(vuelos: List[Vuelo], aeropuertos: List[Aeropuerto]): (String, String) => List[Itinerario] = {
    // Recibe vuelos, una lista de todos los vuelos disponibles y
    // aeropuertos una lista de todos los aeropuertos
    // y devuelve una funcion que recibe c1 y c2, codigos de aeropuertos
    // y devuelve los tres (si los hay) itinerarios que minimizan el tiempo en el aire entre esos dos aeropuertos
    ???
  }

  def itinerarioSalida(vuelos: List[Vuelo], aeropuertos: List[Aeropuerto]): (String, String, Int, Int) => Itinerario = {
    // Recibe vuelos, una lista de todos los vuelos disponibles y
    // aeropuertos una lista de todos los aeropuertos
    // y devuelve una funcion que recibe c1 y c2, codigos de aeropuertos, y h:m una hora de la cita en c2
    // y devuelve el itinerario que optimiza la hora de salida para llegar a tiempo a la cita
    ???
  }

} 
