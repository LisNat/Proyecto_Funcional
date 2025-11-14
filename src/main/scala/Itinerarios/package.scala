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
    // Creamos mapa para búsqueda rápida de aeropuertos
    val aeropuertoMap = aeropuertos.map(a => a.Cod -> a).toMap

    // Función auxiliar para calcular el tiempo total de un itinerario en minutos
    def calcularTiempoTotal(itinerario: Itinerario): Int = {
      if (itinerario.isEmpty) 0
      else {
        def convertirAMinutosUTC(hora: Int, minutos: Int, gmt: Int): Int = {
          val gmtHoras = gmt / 100
          val gmtMinutos = gmt % 100
          val diferenciaGmtEnMinutos = (gmtHoras * 60) + gmtMinutos
          val minutosLocales = hora * 60 + minutos
          minutosLocales - diferenciaGmtEnMinutos
        }

        // Función para calcular duración de un vuelo
        def duracionVuelo(vuelo: Vuelo): Int = {
          (aeropuertoMap.get(vuelo.Org), aeropuertoMap.get(vuelo.Dst)) match {
            case (Some(origen), Some(destino)) =>
              val salidaGMT = convertirAMinutosUTC(vuelo.HS, vuelo.MS, origen.GMT)
              val llegadaGMT = convertirAMinutosUTC(vuelo.HL, vuelo.ML, destino.GMT)
              val duracion = llegadaGMT - salidaGMT
              // Si es negativo, el vuelo llega al día siguiente
              if (duracion < 0) duracion + 24 * 60 else duracion
            case _ =>
              Int.MaxValue // Aeropuerto no encontrado
          }
        }

        // Función para calcular tiempo de espera entre dos vuelos consecutivos
        def tiempoEspera(vuelo1: Vuelo, vuelo2: Vuelo): Int = {
          val llegadaLocal = vuelo1.HL * 60 + vuelo1.ML
          val salidaLocal = vuelo2.HS * 60 + vuelo2.MS
          val espera = salidaLocal - llegadaLocal
          // Si es negativo, el siguiente vuelo sale al día siguiente
          if (espera < 0) espera + 24 * 60 else espera
        }

        // Calculamos la duración total de todos los vuelos
        val tiemposVuelos = itinerario.map(duracionVuelo).sum
        // Calculamos los tiempos de espera entre vuelos consecutivos
        val tiemposEspera =
          (for (i <- 0 until itinerario.length - 1)
            yield tiempoEspera(itinerario(i), itinerario(i+1))).sum

        tiemposVuelos + tiemposEspera
      }
    }

    // Retornamos la función que calcula los itinerarios con menor tiempo
    (cod1: String, cod2: String) => {
      val todosItinerarios = itinerarios(vuelos, aeropuertos)(cod1, cod2)
      // Ordenamos por tiempo total y tomamos los primeros 3
      todosItinerarios.sortBy(calcularTiempoTotal).take(3)
    }
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
