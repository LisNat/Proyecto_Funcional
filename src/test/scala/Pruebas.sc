import Datos._
import helper.Helper
import Itinerarios._
import ItinerariosPar._

val vuelosEvaluado = vuelosD1
val aeropuertosEvaluado = aeropuertos

// Definición de funciones a probar
val itsEscalasPar = itinerariosEscalasPar(vuelosEvaluado, aeropuertosEvaluado)
val itsEscalasSec = itinerariosEscalas(vuelosEvaluado, aeropuertosEvaluado)

val itsAirePar = itinerariosAirePar(vuelosEvaluado, aeropuertosEvaluado)
val itsAireSec = itinerariosAire(vuelosEvaluado, aeropuertosEvaluado)

val itsTiempoPar = itinerariosTiempoPar(vuelosEvaluado, aeropuertosEvaluado)
val itsTiempoSec = itinerariosTiempo(vuelosEvaluado, aeropuertosEvaluado)

val itsSalidaPar = itinerarioSalidaPar(vuelosEvaluado, aeropuertosEvaluado)
val itsSalidaSec = itinerarioSalida(vuelosEvaluado, aeropuertosEvaluado)

val itsPar = itinerariosPar(vuelosEvaluado, aeropuertosEvaluado)
val itsSec = itinerarios(vuelosEvaluado, aeropuertosEvaluado)

//val its1 = itsSec("HOU","MSY")

def generarCombinaciones(aeropuertos: List[Aeropuerto]): List[(Int, String, String)] = {
  val paresOrdenados =
    for {
      (a1, i) <- aeropuertos.zipWithIndex
      (a2, j) <- aeropuertos.zipWithIndex
      if i != j                       // evita pares (A,A)
    } yield (a1.Cod, a2.Cod)

  // Numerar cada par con un índice secuencial
  paresOrdenados.zipWithIndex.map { case ((cod1, cod2), idx) =>
    (idx, cod1, cod2)
  }
}

val pruebas = generarCombinaciones(aeropuertosEvaluado)

/* Logs de funcion generarCombinaciones
pruebas.foreach { case (id, cod1, cod2) =>
  println(s"[$id] $cod1 -> $cod2")
}
*/


def measureTime[T](block: => T): (T, Double) = {
  val start = System.nanoTime()
  val result = block
  val end = System.nanoTime()
  val elapsedMs = (end - start) / 1e6
  (result, elapsedMs) // Convert to ms
}

var totalEscalasPar = 0.0
var totalEscalasSec = 0.0
var totalAirePar = 0.0
var totalAireSec = 0.0
var totalTiempoPar = 0.0
var totalTiempoSec = 0.0
var totalSalidaPar = 0.0
var totalSalidaSec = 0.0
var totalItinPar = 0.0
var totalItinSec = 0.0

val (escPar, tEscPar) = measureTime { itsEscalasPar("HOU", "MSY") }

val headers = List("Prueba", "Origen", "Destino", "Criterio", "N° Itin (Par)", "N° Itin (Sec)", "Coinciden?", "T. Par (ms)", "T. Sec (ms)")
Helper.imprimirCabecera(headers)

for ((id, org, dst) <- pruebas) {
  // Escalas
  val (escPar, tEscPar) = measureTime {
    itsEscalasPar(org, dst)
  }
  val (escSec, tEscSec) = measureTime {
    itsEscalasSec(org, dst)
  }
  totalEscalasPar += tEscPar
  totalEscalasSec += tEscSec
  Helper.imprimirResultado(List(id.toString, org, dst, "Escalas", escPar.length.toString, escSec.length.toString, if (escPar == escSec) "SI" else "NO", tEscPar.toString, tEscSec.toString))

  // Aire

  val (airePar, tAirePar) = measureTime {
    itsAirePar(org, dst)
  }
  val (aireSec, tAireSec) = measureTime {
    itsAireSec(org, dst)
  }
  totalAirePar += tAirePar
  totalAireSec += tAireSec
  Helper.imprimirResultado(List(id.toString, org, dst, "Aire", airePar.length.toString, aireSec.length.toString, if (airePar == aireSec) "SI" else "NO", tAirePar.toString, tAireSec.toString))

  // Tiempo
  val (tiempoPar, tTiempoPar) = measureTime {
    itsTiempoPar(org, dst)
  }
  val (tiempoSec, tTiempoSec) = measureTime {
    itsTiempoSec(org, dst)
  }
  totalTiempoPar += tTiempoPar
  totalTiempoSec += tTiempoSec
  Helper.imprimirResultado(List(id.toString, org, dst, "Tiempo", tiempoPar.length.toString, tiempoSec.length.toString, if (tiempoPar == tiempoSec) "SI" else "NO", tTiempoPar.toString, tTiempoSec.toString))

  // Salida (Cita a las 23:59)
  val citaH = 23
  val citaM = 59
  val (salidaPar, tSalidaPar) = measureTime {
    itinerarioSalidaPar(vuelosEvaluado, aeropuertosEvaluado)(org, dst, citaH, citaM)
  }
  val (salidaSec, tSalidaSec) = measureTime {
    itinerarioSalida(vuelosEvaluado, aeropuertosEvaluado)(org, dst, citaH, citaM)
  }
  totalSalidaPar += tSalidaPar
  totalSalidaSec += tSalidaSec

  // Convertimos el resultado único a lista para contar (0 o 1)
  val countPar = if (salidaPar.isEmpty) 0 else 1
  val countSec = if (salidaSec.isEmpty) 0 else 1

  Helper.imprimirResultado(List(id.toString, org, dst, "Salida", countPar.toString, countSec.toString, if (salidaPar == salidaSec) "SI" else "NO", tSalidaPar.toString, tSalidaSec.toString))

  // Itinerarios (General)
  val (itinPar, tItinPar) = measureTime {
    itsPar(org, dst)
  }
  val (itinSec, tItinSec) = measureTime {
    itsSec(org, dst)
  }
  totalItinPar += tItinPar
  totalItinSec += tItinSec
  Helper.imprimirResultado(List(id.toString, org, dst, "Itinerarios", itinPar.length.toString, itinSec.length.toString, if (itinPar == itinSec) "SI" else "NO", tItinPar.toString, tItinSec.toString))

}

Helper.imprimirCierre()

// Calcular y mostrar resumen
val stats = List(
  ("Escalas", totalEscalasPar, totalEscalasSec, if (totalEscalasPar > 0) totalEscalasSec / totalEscalasPar else 0.0),
  ("Aire", totalAirePar, totalAireSec, if (totalAirePar > 0) totalAireSec / totalAirePar else 0.0),
  ("Tiempo", totalTiempoPar, totalTiempoSec, if (totalTiempoPar > 0) totalTiempoSec / totalTiempoPar else 0.0),
  ("Salida", totalSalidaPar, totalSalidaSec, if (totalSalidaPar > 0) totalSalidaSec / totalSalidaPar else 0.0),
  ("Itinerarios", totalItinPar, totalItinSec, if (totalItinPar > 0) totalItinSec / totalItinPar else 0.0)
)
Helper.imprimirTablaResumen(stats)