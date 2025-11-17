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
    def calcularEscalas(itinerario: Itinerario): Int = {
      // 1. Suma las escalas técnicas informadas en cada vuelo
      val escalasTecnicas = itinerario.map(_.Esc).sum

      // 2. Suma las escalas de conexión (un itinerario de N vuelos tiene N-1 conexiones)
      val escalasPorConexion = if (itinerario.isEmpty) 0 else itinerario.length - 1

      escalasTecnicas + escalasPorConexion
    }

    (cod1: String, cod2: String) => {
      // 1. Inicia la búsqueda para encontrar todos los itinerarios
      val todosLosItinerarios = itinerarios(vuelos, aeropuertos)(cod1, cod2)

      // 2. Ordena todos los itinerarios encontrados usando la función 'calcularEscalas'
      val itinerariosOrdenados = todosLosItinerarios.sortBy(calcularEscalas)

      // 3. Devuelve los 3 mejores (con menos escalas)
      itinerariosOrdenados.take(3)
    }
  }

  def itinerariosAire(vuelos: List[Vuelo], aeropuertos: List[Aeropuerto]): (String, String) => List[Itinerario] = {

    // Mapa para acceso rápido a los aeropuertos
    val aeropuertoMap: Map[String, Aeropuerto] =
      aeropuertos.map(a => a.Cod -> a).toMap

    // Función para calcular distancia Euclidiana entre dos aeropuertos
    def distancia(vuelo: Vuelo): Double = {
      (aeropuertoMap.get(vuelo.Org), aeropuertoMap.get(vuelo.Dst)) match {
        case (Some(a1), Some(a2)) =>
          val dx = a2.X - a1.X
          val dy = a2.Y - a1.Y
          math.sqrt(dx * dx + dy * dy)
        case _ =>
          Double.MaxValue   // Si no encuentra aeropuertos, penalizar
      }
    }

    // Suma de distancias para un itinerario
    def distanciaTotal(it: Itinerario): Double =
      it.map(distancia).sum

    // Función principal que se retorna
    (cod1: String, cod2: String) => {

      // 1. Obtener todos los itinerarios posibles
      val todos = itinerarios(vuelos, aeropuertos)(cod1, cod2)

      // 2. Ordenar por la distancia total en el aire
      val ordenados = todos.sortBy(distanciaTotal)

      // 3. Devolver los 3 mejores
      ordenados.take(3)
    }
  }


  def itinerarioSalida(vuelos: List[Vuelo], aeropuertos: List[Aeropuerto]): (String, String, Int, Int) => Itinerario = {
    // Recibe vuelos, una lista de todos los vuelos disponibles y
    // aeropuertos una lista de todos los aeropuertos
    // y devuelve una funcion que recibe c1 y c2, codigos de aeropuertos, y h:m una hora de la cita en c2
    // y devuelve el itinerario que optimiza la hora de salida para llegar a tiempo a la cita
    ???
  }

} 
