import Datos._
import ItinerariosPar._
// import scala.util.Random
import org.scalameter._

def tiempoDe[T](body: => T) = {
  val timeA1 = config(
    KeyValue(Key.exec.minWarmupRuns -> 20),
    KeyValue(Key.exec.maxWarmupRuns -> 60),
    KeyValue(Key.verbose -> false)
  ) withWarmer(new Warmer.Default) measure (body)
  timeA1
}

// val random = new Random()

// Ejemplo curso pequeño
val itsCursoPar = itinerariosPar(vuelosCurso,aeropuertosCurso)
//2.1 Aeropuertos incomunicados
val its1 = itsCursoPar("MID", "SVCS")
val its2 = itsCursoPar("CLO", "SVCS")

// 4 itinerarios CLO-SVO
val its3 = itsCursoPar("CLO","SVO")

//2 itinerarios CLO-MEX
val its4 = itsCursoPar("CLO", "MEX")

//2 itinerarios CTG-PTY
val its5 = itsCursoPar("CTG","PTY")

tiempoDe(itsCursoPar("CLO","SVO"))

val itsTiempoCurso = itinerariosTiempoPar(vuelosCurso,aeropuertosCurso)

// prueba itinerariosTiempo
val itst1 = itsTiempoCurso("MID", "SVCS")
val itst2 = itsTiempoCurso("CLO", "SVCS")

// 4 itinerarios CLO-SVO
val itst3 = itsTiempoCurso("CLO","SVO")

//2 itinerarios CLO-MEX
val itst4 = itsTiempoCurso("CLO", "MEX")

//2 itinerarios CTG-PTY
val itst5 = itsTiempoCurso("CTG","PTY")

tiempoDe(itsTiempoCurso("MID", "SVCS"))
tiempoDe(itsTiempoCurso("CLO", "SVCS"))
tiempoDe(itsTiempoCurso("CLO","SVO"))
tiempoDe(itsTiempoCurso("CTG","PTY"))
tiempoDe(itsTiempoCurso("CTG","PTY"))

// prueba itinerariosEscalas
val itsEscalasCurso = itinerariosEscalasPar(vuelosCurso,aeropuertosCurso)

val itsc1 = itsEscalasCurso("MID", "SVCS")
val itsc2 = itsEscalasCurso("CLO", "SVCS")

// 4 itinerarios CLO-SVO
val itsc3 = itsEscalasCurso("CLO","SVO")

//2 itinerarios CLO-MEX
val itsc4 = itsEscalasCurso("CLO", "MEX")

//2 itinerarios CTG-PTY
val itsc5 = itsEscalasCurso("CTG","PTY")

// prueba itinerariosAire
val itsAireCurso = itinerariosAirePar(vuelosCurso,aeropuertosCurso)

val itsa1 = itsAireCurso("MID", "SVCS")
val itsa2 = itsAireCurso("CLO", "SVCS")

// 4 itinerarios CLO-SVO
val itsa3 = itsAireCurso("CLO","SVO")

//2 itinerarios CLO-MEX
val itsa4 = itsAireCurso("CLO", "MEX")

//2 itinerarios CTG-PTY
val itsa5 = itsAireCurso("CTG","PTY")


// prueba itinerarioSalida
val itSalidaCurso = itinerarioSalidaPar(vuelosCurso,aeropuertosCurso)

val itsal1 = itSalidaCurso("CTG","PTY",11, 40)
val itsal2 = itSalidaCurso("CTG","PTY",11, 55)
val itsal3 = itSalidaCurso("CTG","PTY",10, 30)

val itsPar15A1 = itinerariosPar(vuelosA1,aeropuertos)
val itsTpoPar15A1 = itinerariosTiempoPar(vuelosA1,aeropuertos)
val itsEscPar15A1 = itinerariosEscalasPar(vuelosA1,aeropuertos)
val itsAirPar15A1 = itinerariosAirePar(vuelosA1,aeropuertos)
val itsSalPar15A1 = itinerarioSalidaPar(vuelosA1,aeropuertos)
itsPar15A1("HOU","BNA")
itsTpoPar15A1("HOU","BNA")
itsEscPar15A1("HOU","BNA")
itsAirPar15A1("HOU","BNA")
itsSalPar15A1("HOU","BNA", 18, 30)

val itsPar40B1 = itinerariosPar(vuelosB1,aeropuertos)
val itsTpoPar40B1 = itinerariosTiempoPar(vuelosB1,aeropuertos)
val itsEscPar40B1 = itinerariosEscalasPar(vuelosB1,aeropuertos)
val itsAirPar40B1 = itinerariosAirePar(vuelosB1,aeropuertos)
val itsSalPar40B1 = itinerarioSalidaPar(vuelosB1,aeropuertos)
itsPar40B1("DFW","ORD")
itsTpoPar40B1("DFW","ORD")
itsEscPar40B1("DFW","ORD")
itsAirPar40B1("DFW","ORD")
itsSalPar40B1("DFW","ORD", 18, 30)

val itsPar100C1 = itinerariosPar(vuelosC1,aeropuertos)
val itsTpoPar100C1 = itinerariosTiempoPar(vuelosC1,aeropuertos)
val itsEscPar100C1 = itinerariosEscalasPar(vuelosC1,aeropuertos)
val itsAirPar100C1 = itinerariosAirePar(vuelosC1,aeropuertos)
val itsSalPar100C1 = itinerarioSalidaPar(vuelosC1,aeropuertos)
itsPar100C1("ORD","TPA")
itsTpoPar100C1("ORD","TPA")
itsEscPar100C1("ORD","TPA")
itsAirPar100C1("ORD","TPA")
itsSalPar100C1("ORD","TPA", 18, 30)

val itsPar200C = itinerariosPar(vuelosC1++vuelosC2, aeropuertos)
val itsTpoPar200C = itinerariosTiempoPar(vuelosC1++vuelosC2, aeropuertos)
val itsEscPar200C = itinerariosEscalasPar(vuelosC1++vuelosC2, aeropuertos)
val itsAirPar200C = itinerariosAirePar(vuelosC1++vuelosC2, aeropuertos)
val itsSalPar200C = itinerarioSalidaPar(vuelosC1++vuelosC2, aeropuertos)
itsPar200C("ORD","TPA")
itsTpoPar200C("ORD","TPA")
itsEscPar200C("ORD","TPA")
itsAirPar200C("ORD","TPA")
itsSalPar200C("ORD","TPA", 18, 30)


val its300CPar = itinerariosPar((vuelosC1++vuelosC2++vuelosC3), aeropuertos)
its300CPar("ORD","TPA")

val its400CPar = itinerariosPar((vuelosC1++vuelosC2++vuelosC3++vuelosC4), aeropuertos)
its400CPar("ORD","TPA")

val its500CPar = itinerariosPar((vuelosC1++vuelosC2++vuelosC3++vuelosC4++vuelosC5), aeropuertos)
its500CPar("ORD","TPA")
