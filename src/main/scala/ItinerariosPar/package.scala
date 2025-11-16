import Datos._
import common._
import Itinerarios._

import scala.collection.parallel.CollectionConverters._
import scala.collection.parallel.ParSeq

package object ItinerariosPar {
  def itinerariosPar(vuelos: List[Vuelo], aeropuertos: List[Aeropuerto]): (String, String) => List[Itinerario] = {
    val vuelosPorOrigen = vuelos.groupBy(_.Org)
    val codsValidos = aeropuertos.map(_.Cod).toSet

    def buscarItinerariosPar(cod1: String, cod2: String, visitados: Set[String]): List[Itinerario] = {
      // Caso base: llegó al destino
      if (cod1 == cod2) {
        List(Nil)
      } else {
        val vuelosSalientes = vuelosPorOrigen.getOrElse(cod1, Nil)
        val vuelosValidos = vuelosSalientes.filter(v => !visitados.contains(v.Dst))

        // Umbral: Si hay 1 o 0 vuelos, es más rápido hacerlo secuencial.
        if (vuelosValidos.length <= 1) {
          for {
            vuelo <- vuelosValidos
            resto <- buscarItinerariosPar(vuelo.Dst, cod2, visitados + cod1)
          } yield vuelo :: resto
        } else {
          // Cada vuelo se procesa en su propia tarea.
          val tasks = for (vuelo <- vuelosValidos)
            yield task {
              val subItinerarios = buscarItinerariosPar(vuelo.Dst, cod2, visitados + cod1)
              subItinerarios.map(resto => vuelo :: resto)
            }
          (for (t <- tasks) yield t.join()).flatten
        }
      }
    }

    (cod1: String, cod2: String) => {
      if (!codsValidos.contains(cod1) || !codsValidos.contains(cod2)) {
        List()
      } else {
        buscarItinerariosPar(cod1, cod2, Set())
      }
    }
  }

  def itinerariosTiempoPar(vuelos: List[Vuelo], aeropuertos: List[Aeropuerto]): (String, String) => List[Itinerario] = {
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
      val todosItinerarios = itinerariosPar(vuelos, aeropuertos)(cod1, cod2)
      // Ordenamos por tiempo total y tomamos los primeros 3
      todosItinerarios.sortBy(calcularTiempoTotal).take(3)
    }
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
