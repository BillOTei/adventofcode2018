package days

import scala.collection.mutable.ArrayBuffer
import scala.io.Source

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
            .map(p => {
              p._1 match {
                case _ @ ">" | "<" =>
                  carts.+=((p._2, l._2, p._1, ("l", "s", "r")))
                  (p._2, l._2, "-")
                case _ @ "v" | "^" =>
                  carts.+=((p._2, l._2, p._1, ("l", "s", "r")))
                  (p._2, l._2, "|")
                case _ => (p._2, l._2, p._1)
              }
            }))
      .toArray
  }

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

  private def hasCrashed(
      carts: ArrayBuffer[(Int, Int, String, (String, String, String))]) = {
    val cartsByPos = carts.map(c => ((c._1, c._2), c._3)).groupBy(_._1)

    cartsByPos.exists(_._2.length > 1)
  }

  def part1(): (Int, Int) = {
    val s = source()
    var localCarts = this.carts.sortBy(r => (r._2, r._1))

    while (true) {
      for (i <- localCarts.indices) {
        localCarts(i) = move(localCarts(i), s)
        if (hasCrashed(localCarts)) {
          val cartsByPos = localCarts.map(c => ((c._1, c._2), c._3)).groupBy(_._1)

          return cartsByPos.find(_._2.length > 1).get._1
        }
      }
      localCarts = localCarts.sortBy(r => (r._2, r._1))
    }

    (0, 0)
  }

  def part2() = {}
}
