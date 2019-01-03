package days

import scala.collection.mutable.ArrayBuffer
import scala.io.Source
import scala.util.Try

object Day13 {
  private val carts =
    ArrayBuffer[(Int, Int, String, (String, String, String))]()

  private def source(): Array[Array[(Int, Int, String)]] = {
    Source
      .fromFile("sources/source13.txt")
      .getLines
      .zipWithIndex
      .map(
        l =>
          l._1
            .split("")
            .zipWithIndex
            .filter(_._1 != " ")
            .map(p => {
              if (List("<", ">", "^", "v").contains(p._1)) {
                carts.+=((p._2, l._2, p._1, ("l", "s", "r")))
              }

              (p._2, l._2, p._1)
            }))
      .toArray
  }
  //.foreach(l => l.foreach(println))

  private def process(p: (Int, Int, String),
                      c: (Int, Int, String, (String, String, String))) =
    p._3 match {
      case "-"  => (p._1, p._2, c._3, c._4)
      case "\\" => (p._1, p._2, turn(c._3, p._3), c._4)
      case "|"  => (p._1, p._2, c._3, c._4)
      case "/"  => (p._1, p._2, turn(c._3, p._3), c._4)
      case "+"  => (p._1, p._2, turn(c._3, c._4._1), (c._4._2, c._4._3, c._4._1))
    }

  private def turn(c: String, d: String) = (c, d) match {
    case (">", "\\") => "v"
    case (">", "/")  => "^"
    case (">", "l")  => "^"
    case (">", "s")  => ">"
    case (">", "r")  => "v"

    case ("v", "\\") => ">"
    case ("v", "/")  => "<"
    case ("v", "l")  => ">"
    case ("v", "s")  => "v"
    case ("v", "r")  => "<"

    case ("<", "\\") => "^"
    case ("<", "/")  => "v"
    case ("<", "l")  => "v"
    case ("<", "s")  => "<"
    case ("<", "r")  => "^"

    case ("^", "\\") => "<"
    case ("^", "/")  => ">"
    case ("^", "l")  => "<"
    case ("^", "s")  => "^"
    case ("^", "r")  => ">"
  }

  private def move(c: (Int, Int, String, (String, String, String)),
                   source: Array[Array[(Int, Int, String)]]) = c._3 match {
    case ">" => Try(source(c._2)).flatMap(row => Try(row(c._1 + 1)).map(p => process(p, c)))
    case "v" =>
    case "<" =>
    case "^" =>
  }

  def part1() = {
    val s = source()
    def go(source: Array[Array[(Int, Int, String)]]): Unit = {
      carts.map(c => {
        val t = Try(source(c._2)).flatMap(row =>
          Try(row(c._1 + 1)).map(p => process(p, c)))

        t
      })

      go(
        source
      )
    }
  }

  def part2() = {}
}
