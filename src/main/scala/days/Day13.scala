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
    case ">" => process(source(c._2)(c._1 + 1), c)
    case "v" => process(source(c._2 + 1)(c._1), c)
    case "<" => process(source(c._2)(c._1 - 1), c)
    case "^" => process(source(c._2 - 1)(c._1), c)
  }

  def part1() = {
    val s = source()
    def go(source: Array[Array[(Int, Int, String)]],
           carts: ArrayBuffer[(Int, Int, String, (String, String, String))])
      : (Int, Int) = {
      val cartsByPos = carts.map(c => ((c._1, c._2), c._3)).groupBy(_._1)
      if (cartsByPos.exists(_._2.length > 1)) {
        cartsByPos.find(_._2.length > 1).get._1
      } else {
        go(
          source,
          carts.map(c => move(c, source))
        )
      }
    }

    go(s, carts)
  }

  def part2() = {}
}
