package helper
import Datos._

object Helper {
  private val colWidths = List(8, 8, 8, 10, 15, 15, 12, 12, 12)
  private val separator = colWidths.map("-" * _).mkString("+", "+", "+")

  private def formatRow(row: List[String]): String = {
    row.zip(colWidths).map { case (cell, width) =>
      val cellStr = if (cell.length > width) cell.take(width) else cell
      cellStr.padTo(width, ' ')
    }.mkString("|", "|", "|")
  }

  def imprimirCabecera(headers: List[String]): Unit = {
    println(separator)
    println(formatRow(headers))
    println(separator)
  }

  def imprimirResultado(row: List[String]): Unit = {
    println(formatRow(row))
  }

  def imprimirCierre(): Unit = {
    println(separator)
  }

  def imprimirTablaResumen(stats: List[(String, Double, Double, Double)]): Unit = {
    val headers = List("Algoritmo", "T. Par (ms)", "T. Sec (ms)", "Speedup")
    val widths = List(15, 15, 15, 10)
    val sep = widths.map("-" * _).mkString("+", "+", "+")

    val format = (row: List[String]) =>
      row.zip(widths).map { case (cell, width) =>
        val cellStr = if (cell.length > width) cell.take(width) else cell
        cellStr.padTo(width, ' ')
      }.mkString("|", "|", "|")

    println("\nRESUMEN DE RENDIMIENTO")
    println(sep)
    println(format(headers))
    println(sep)
    stats.foreach { case (alg, tp, ts, su) =>
      println(format(List(alg, tp.toString, ts.toString, f"$su%2.2f")))
    }
    println(sep)
  }
}