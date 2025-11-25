import Datos._
import common._
import Itinerarios._

import scala.collection.parallel.CollectionConverters._
import scala.collection.parallel.ParSeq

package object ItinerariosPar {
  def itinerariosPar(vuelos: List[Vuelo], aeropuertos: List[Aeropuerto]): (String, String) => List[Itinerario] = {
    val vuelosPorOrigen = vuelos.groupBy(_.Org)
    val codsValidos = aeropuertos.map(_.Cod).toSet

    // Añadimos límite de conexiones para prevenir OUT OF MEMORY
    val MAX_CONEXIONES = 4

    def buscarItinerariosPar(cod1: String, cod2: String, visitados: Set[String], conexiones: Int): List[Itinerario] = {
      // Caso base: llegó al destino
      if (cod1 == cod2) {
        List(Nil)
      } // CONDICIÓN DE PARADA: Si excedemos las conexiones, devolvemos List()
      else if (conexiones >= MAX_CONEXIONES) {
        List()
      } else {
        val vuelosSalientes = vuelosPorOrigen.getOrElse(cod1, Nil)
        val vuelosValidos = vuelosSalientes.filter(v => !visitados.contains(v.Dst))

        // Umbral: Si hay 1 o 0 vuelos, es más rápido hacerlo secuencial.
        if (vuelosValidos.length <= 1) {
          for {
            vuelo <- vuelosValidos
            resto <- buscarItinerariosPar(vuelo.Dst, cod2, visitados + cod1, conexiones + 1)
          } yield vuelo :: resto
        } else {
          // Cada vuelo se procesa en su propia tarea.
          val tasks = for (vuelo <- vuelosValidos)
            yield task {
              val subItinerarios = buscarItinerariosPar(vuelo.Dst, cod2, visitados + cod1, conexiones + 1)
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
        buscarItinerariosPar(cod1, cod2, Set(), 0)
      }
    }
  }

  def itinerariosTiempoPar(vuelos: List[Vuelo], aeropuertos: List[Aeropuerto]): (String, String) => List[Itinerario] = {
    val aeropuertoMap = aeropuertos.map(a => a.Cod -> a).toMap
    val minutosPorDia = 24 * 60 // 1440

    // Función auxiliar para calcular el tiempo total de un itinerario en minutos
    def calcularTiempoTotal(itinerario: Itinerario): Int = {
      if (itinerario.isEmpty) 0
      else {
        // Función para obtener las horas UTC de salida y llegada para un vuelo (en ciclo de 24h)
        def obtenerHorasUTC(v: Vuelo): (Int, Int) = {
          val origen = aeropuertoMap(v.Org)
          val destino = aeropuertoMap(v.Dst)
          val salida = convertirAMinutosUTC(v.HS, v.MS, origen.GMT)
          val llegadaBase = convertirAMinutosUTC(v.HL, v.ML, destino.GMT)
          // Si llegadaBase <= salida, el vuelo dura al menos un día
          val llegada = if (llegadaBase > salida) llegadaBase else llegadaBase + minutosPorDia
          (salida, llegada)
        }

        val primerVuelo = itinerario.head
        val (salidaInicialUTC, llegadaInicialUTC) = obtenerHorasUTC(primerVuelo)

        // Tiempo Total Acumulado, Hora de Llegada Absoluta del Vuelo Anterior
        val estadoInicial: (Int, Int) = (llegadaInicialUTC - salidaInicialUTC, llegadaInicialUTC)

        // Acumulación usando foldLeft para manejar el estado secuencial
        val (tiempoTotal, _) = itinerario.tail.foldLeft(estadoInicial) {
          case ((minutosAcumulados, llegadaPrevAbsoluta), vueloActual) =>
            val (salidaActualUTC, llegadaActualUTC) = obtenerHorasUTC(vueloActual)

            val minutosDiferencia = llegadaPrevAbsoluta - salidaActualUTC
            val diasCompletosPasados = minutosDiferencia / minutosPorDia

            val salidaTentativa = salidaActualUTC + (diasCompletosPasados * minutosPorDia)

            val salidaAbsoluta =
              if (salidaTentativa < llegadaPrevAbsoluta) salidaTentativa + minutosPorDia
              else salidaTentativa

            val tiempoEspera = salidaAbsoluta - llegadaPrevAbsoluta // Espera real
            val duracionVuelo = llegadaActualUTC - salidaActualUTC

            val nuevoTiempoTotal = minutosAcumulados + tiempoEspera + duracionVuelo
            val nuevaLlegadaAbsoluta = salidaAbsoluta + duracionVuelo

            (nuevoTiempoTotal, nuevaLlegadaAbsoluta)
        }
        tiempoTotal
      }
    }
    // Retornamos la función que calcula los itinerarios con menor tiempo
    (cod1: String, cod2: String) => {
      // Obtener itinerarios con la versión paralela
      val todosItinerarios = itinerariosPar(vuelos, aeropuertos)(cod1, cod2)

      // Si hay pocos itinerarios, mejor secuencial
      if (todosItinerarios.size <= 2)
        todosItinerarios.sortBy(calcularTiempoTotal).take(3)
      else {
        // Paralelizamos el cálculo del tiempo de *cada itinerario*
        val tasks = todosItinerarios.map { it =>
          task {
            (it, calcularTiempoTotal(it))
          }
        }
        // Recoger resultados
        val resultados = tasks.map(_.join())
        // Ordenar por tiempo total
        resultados.sortBy(_._2).map(_._1).take(3)
      }
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
      val todosItinerarios = itinerariosPar(vuelos, aeropuertos)(cod1, cod2)

      // 2. Calcular el puntaje de escalas en paralelo usando 'task'
      val tasks = todosItinerarios.map { it =>
        task {
          (it, calcularEscalasPar(it))
        }
      }

      // 3. Recoger resultados (join)
      val resultados = tasks.map(_.join())

      // 4. Ordenar por número de escalas (segundo elemento de la tupla) y tomar los 3 mejores
      resultados.sortBy(_._2).map(_._1).take(3)
    }
  }

  def itinerariosAirePar(vuelos: List[Vuelo], aeropuertos: List[Aeropuerto]): (String, String) => List[Itinerario] = {
    // Mapa inmutable para acceso rápido
    val aeropuertoMap = aeropuertos.map(a => a.Cod -> a).toMap

    // Tiempo de vuelo real entre dos aeropuertos
    def duracionVuelo(vuelo: Vuelo): Int = {
      (aeropuertoMap.get(vuelo.Org), aeropuertoMap.get(vuelo.Dst)) match {
        case (Some(origen), Some(destino)) =>
          val salidaUTC = convertirAMinutosUTC(vuelo.HS, vuelo.MS, origen.GMT)
          val llegadaUTC = convertirAMinutosUTC(vuelo.HL, vuelo.ML, destino.GMT)
          val d = llegadaUTC - salidaUTC
          if (d < 0) d + 24 * 60 else d
        case _ =>
          Int.MaxValue
      }
    }

    // Tiempo total en aire de un itinerario
    // PARALLEL HERE → calcula duración de cada vuelo en paralelo
    def tiempoEnAireTotal(it: Itinerario): Int =
      it.par.map(duracionVuelo).sum

    // Función que se retorna
    (cod1: String, cod2: String) => {

      // Obtener itinerarios secuencialmente (el proyecto NO pide paralelizar esta parte)
      val todos = itinerariosPar(vuelos, aeropuertos)(cod1, cod2)

      // SEGUNDO NIVEL DE PARALELISMO → calcular tiempos en paralelo para cada itinerario
      val tiemposPar =
        todos.par.map(it => (it, tiempoEnAireTotal(it))).toList

      // Ordenamos secuencialmente (colección ya pequeña)
      val ordenados =
        tiemposPar.sortBy(_._2).take(3).map(_._1)

      ordenados
    }
  }

  def itinerarioSalidaPar(vuelos: List[Vuelo], aeropuertos: List[Aeropuerto]): (String, String, Int, Int) => Itinerario = {
    // Creamos mapa para búsqueda rápida de aeropuertos
    val aeropuertoMap = aeropuertos.map(a => a.Cod -> a).toMap

    // Función auxiliar para convertir hora local a minutos UTC
    def convertirAMinutosUTC(hora: Int, minutos: Int, gmt: Int): Int = {
      val gmtHoras = gmt / 100
      val gmtMinutos = gmt % 100
      val diferenciaGmtEnMinutos = (gmtHoras * 60) + gmtMinutos
      val minutosLocales = hora * 60 + minutos
      minutosLocales - diferenciaGmtEnMinutos
    }

    // Función auxiliar para obtener la hora de salida de un itinerario en minutos UTC
    def horaSalida(itinerario: Itinerario): Int = {
      if (itinerario.isEmpty) Int.MaxValue
      else {
        val primerVuelo = itinerario.head
        aeropuertoMap.get(primerVuelo.Org) match {
          case Some(aeropuerto) =>
            convertirAMinutosUTC(primerVuelo.HS, primerVuelo.MS, aeropuerto.GMT)
          case None => Int.MaxValue
        }
      }
    }

    // Función auxiliar para obtener la hora de llegada de un itinerario en minutos UTC
    def horaLlegada(itinerario: Itinerario): Int = {
      if (itinerario.isEmpty) Int.MaxValue
      else {
        val ultimoVuelo = itinerario.last
        aeropuertoMap.get(ultimoVuelo.Dst) match {
          case Some(aeropuerto) =>
            convertirAMinutosUTC(ultimoVuelo.HL, ultimoVuelo.ML, aeropuerto.GMT)
          case None => Int.MaxValue
        }
      }
    }

    // Función auxiliar para calcular el tiempo total del itinerario (para desempatar)
    def tiempoTotal(itinerario: Itinerario): Int = {
      if (itinerario.isEmpty) Int.MaxValue
      else {
        val salida = horaSalida(itinerario)
        val llegada = horaLlegada(itinerario)
        val duracion = llegada - salida
        // Si la duración es negativa, significa que cruza la medianoche
        if (duracion < 0) duracion + 24 * 60 else duracion
      }
    }

    // Retornamos la función que calcula el itinerario con la salida más tardía
    (cod1: String, cod2: String, h: Int, m: Int) => {
      // Obtener todos los itinerarios posibles usando la versión paralela
      val todosItinerarios = itinerariosPar(vuelos, aeropuertos)(cod1, cod2)


      // Convertir la hora de la cita a minutos UTC del aeropuerto de destino
      val horaCitaUTC = aeropuertoMap.get(cod2) match {
        case Some(aeropuerto) => convertirAMinutosUTC(h, m, aeropuerto.GMT)
        case None => Int.MinValue // Si el aeropuerto no existe, ningún itinerario será válido
      }

      // Función para calcular cuántos "días antes" llega un itinerario
      // Retorna: 0 = mismo día, 1 = día anterior, 2 = dos días antes, etc.
      def diasAntes(llegada: Int, cita: Int): Int = {
        if (llegada <= cita) 0  // Llega el mismo día antes de la cita
        else {
          // Necesita llegar días antes
          val diff = llegada - cita
          val dias = (diff / (24 * 60)) + 1
          dias
        }
      }


      // Si no hay itinerarios, retornar lista vacía
      if (todosItinerarios.isEmpty) List()
      else {
        // Calculamos los criterios de ordenamiento en paralelo
        val tasks = todosItinerarios.map { it =>
          task {
            val llegada = horaLlegada(it)
            val salida = horaSalida(it)
            val duracion = tiempoTotal(it)
            val dias = diasAntes(llegada, horaCitaUTC)

            // Retornamos el itinerario junto con la tupla de comparación
            (it, (dias, -salida, duracion))
          }
        }

        val resultados = tasks.map(_.join())

        // Ordenamos por la tupla pre-calculada y devolvemos el mejor itinerario
        resultados.sortBy(_._2).head._1
      }
    }
  }



}
