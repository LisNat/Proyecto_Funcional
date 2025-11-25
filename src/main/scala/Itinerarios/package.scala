import Datos._
package object Itinerarios {

  def itinerarios(vuelos: List[Vuelo], aeropuertos: List[Aeropuerto]): (String, String) => List[Itinerario] = {
    val vuelosPorOrigen = vuelos.groupBy(_.Org)
    val codsValidos = aeropuertos.map(_.Cod).toSet

    // Añadimos límite de conexiones para prevenir OUT OF MEMORY
    val MAX_CONEXIONES = 4

    // Función recursiva que busca todos los itinerarios desde un aeropuerto de origen (cod1) hasta un destino (cod2)
    def buscarItinerarios(cod1: String, cod2: String, visitados: Set[String], conexiones: Int): List[Itinerario] = {
      if (cod1 == cod2) {
        List(Nil)
      } // CONDICIÓN DE PARADA: Si excedemos las conexiones, devolvemos List()
      else if (conexiones >= MAX_CONEXIONES) {
        List()
      } else {
        // Obtenemos los vuelos que salen del aeropuerto actual
        val vuelosSalientes = vuelosPorOrigen.getOrElse(cod1, Nil)
        // Y filtramos los que conducen a aeropuertos aún no visitados
        val vuelosValidos = vuelosSalientes.filter(v => !visitados.contains(v.Dst))

        // Exploramos recursivamente los destinos posibles, concatenando el vuelo actual con el resto del itinerario
        for {
          vuelo <- vuelosValidos
          resto <- buscarItinerarios(vuelo.Dst, cod2, visitados + cod1, conexiones + 1)
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
        buscarItinerarios(cod1, cod2, Set(), 0)
      }
    }
  }

  def convertirAMinutosUTC(hora: Int, minutos: Int, gmt: Int): Int = {
    val gmtHoras = gmt / 100
    val gmtMinutos = gmt % 100
    val diferenciaGmtEnMinutos = (gmtHoras * 60) + gmtMinutos
    val minutosLocales = hora * 60 + minutos
    minutosLocales - diferenciaGmtEnMinutos
  }

  def itinerariosTiempo(vuelos: List[Vuelo], aeropuertos: List[Aeropuerto]): (String, String) => List[Itinerario] = {
    // Creamos mapa para búsqueda rápida de aeropuertos
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

        // Acumulacion usando foldLeft para manejar el estado secuencial
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

    val aeropuertoMap = aeropuertos.map(a => a.Cod -> a).toMap

    def duracionVuelo(vuelo: Vuelo): Int = {
      (aeropuertoMap.get(vuelo.Org), aeropuertoMap.get(vuelo.Dst)) match {
        case (Some(origen), Some(destino)) =>
          val salidaGMT = convertirAMinutosUTC(vuelo.HS, vuelo.MS, origen.GMT)
          val llegadaGMT = convertirAMinutosUTC(vuelo.HL, vuelo.ML, destino.GMT)
          val duracion = llegadaGMT - salidaGMT
          if (duracion < 0) duracion + 24 * 60 else duracion
        case _ =>
          Int.MaxValue
      }
    }

    def tiempoEnAireTotal(it: Itinerario): Int =
      it.map(duracionVuelo).sum

    (cod1: String, cod2: String) => {
      val todos = itinerarios(vuelos, aeropuertos)(cod1, cod2)
      todos.sortBy(tiempoEnAireTotal).take(3)
    }
  }


  def itinerarioSalida(vuelos: List[Vuelo], aeropuertos: List[Aeropuerto]): (String, String, Int, Int) => Itinerario = {
    val aeropuertoMap = aeropuertos.map(a => a.Cod -> a).toMap

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
      // Obtener todos los itinerarios posibles
      val todosItinerarios = itinerarios(vuelos, aeropuertos)(cod1, cod2)

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
      if (todosItinerarios.isEmpty) {
        List()
      } else {
        // Ordenar por:
        // 1. Mínimo número de días antes (0 es mejor que 1, 1 es mejor que 2)
        // 2. Hora de salida más tardía (descendente)
        // 3. Tiempo total menor (desempate)
        val ordenados = todosItinerarios.sortBy { itinerario =>
          val llegada = horaLlegada(itinerario)
          val dias = diasAntes(llegada, horaCitaUTC)
          (dias, -horaSalida(itinerario), tiempoTotal(itinerario))
        }

        // Retornar el primero (el óptimo)
        ordenados.head
      }
    }
  }


} 
