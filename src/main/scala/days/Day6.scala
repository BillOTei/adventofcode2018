package days

import scala.io.Source

object Day6 {
  private lazy val list = Source.fromFile("sources/source6.txt").getLines()
    .map(_.split(", "))
    .map(c => (c(0).toInt, c(1).toInt))
    .toList
    .sortBy(_._2)
  private lazy val xExtremes = List(list.minBy(_._1)._1, list.maxBy(_._1)._1)
  private lazy val yExtremes = List(list.minBy(_._2)._2, list.maxBy(_._2)._2)
  private lazy val cleanList = list.filter(c => !xExtremes.contains(c._1) && !yExtremes.contains(c._2))
  private lazy val matrix = for {
    x <- xExtremes.head to xExtremes(1)
    y <- yExtremes.head to yExtremes(1)
  } yield (x, y)

  def part1(): Int = {
    matrix.map {
      case (x, y) if !list.contains((x, y)) =>
        list.map(p => (p, manhatan((x, y), p), (x, y)))
          .groupBy(_._2)

      case _ => Map.empty
    }
      .filterNot(_.isEmpty)
      .map(_.toList.minBy(_._1))
      .filter(_._2.length == 1)
      .flatMap(_._2)
      .groupBy(_._1)
      .mapValues(_.size + 1)
      .filter(p => cleanList.contains(p._1))
      .maxBy(_._2)._2
  }

  private def manhatan(a: (Int, Int), b: (Int, Int)) = Math.abs(a._1 - b._1) + Math.abs(a._2 - b._2)

  def part2(): Int = {
    matrix.map(point =>
      (point, list.map(p => manhatan(point, p)).sum)).count(_._2 < 10000)
  }
}
