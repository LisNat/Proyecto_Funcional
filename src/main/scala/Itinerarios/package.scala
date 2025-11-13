import Datos._
package object Itinerarios {

  def itinerarios(vuelos: List[Vuelo], aeropuertos: List[Aeropuerto]): (String, String) => List[Itinerario] = {
    val vuelosPorOrigen = vuelos.groupBy(_.Org)
    val codsValidos = aeropuertos.map(_.Cod).toSet

    // Función recursiva que busca todos los itinerarios desde un aeropuerto de origen (cod1) hasta un destino (cod2)
    def buscarItinerarios(cod1: String, cod2: String, visitados: Set[String]): List[Itinerario] = {
      if (cod1 == cod2) {
        List(Nil)
      } else {
        // Obtenemos los vuelos que salen del aeropuerto actual
        val vuelosSalientes = vuelosPorOrigen.getOrElse(cod1, Nil)
        // Y filtramos los que conducen a aeropuertos aún no visitados
        val vuelosValidos = vuelosSalientes.filter(v => !visitados.contains(v.Dst))

        // Exploramos recursivamente los destinos posibles, concatenando el vuelo actual con el resto del itinerario
        for {
          vuelo <- vuelosValidos
          resto <- buscarItinerarios(vuelo.Dst, cod2, visitados + cod1)
        } yield vuelo :: resto
      }
    }

    // Función que devolvemos
    (cod1: String, cod2: String) => {
      // Verificamos que ambos códigos correspondan a aeropuertos válidos
      if (!codsValidos.contains(cod1) || !codsValidos.contains(cod2)) {
        List()
      } else {
        // Si es válido, tons iniciamos la búsqueda recursiva desde el aeropuerto de origen hacia el destino
        buscarItinerarios(cod1, cod2, Set())
      }
    }
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
