package days

import scala.io.Source

object Day6 {
  private lazy val list = Source.fromFile("sources/source6.txt").getLines()
    .map(_.split(", "))
    .map(c => (c(0).toInt, c(1).toInt))
    .toList
    .sortBy(_._2)

  def part1() = {
    val xExtremes = List(list.minBy(_._1)._1, list.maxBy(_._1)._1)
    val yExtremes = List(list.minBy(_._2)._2, list.maxBy(_._2)._2)
    //val cleanList = list.filter(c => !xExtremes.contains(c._1) && !yExtremes.contains(c._2))

    val matrix = for {
      x <- xExtremes.head to xExtremes(1)
      y <- yExtremes.head to yExtremes(1)
    } yield (x, y)

    val r = matrix.map {
      case (x, y) if !list.contains((x, y)) =>
        list.map(p => (p, manhatan((x, y), p), (x, y)))
          //.groupBy(_._2)
          //.toList
          //.filter(_._2.length == 1)

      case _ => Nil
    }.foreach(println)

    //r.filterNot(_.isEmpty)
      //.flatMap(_.minBy(_._1)._2)
      //.groupBy(_._1)
      //.mapValues(_.length)
      //.foreach(println)
  }

  private def manhatan(a: (Int, Int), b: (Int, Int)) = Math.abs(a._1 - b._2) + Math.abs(a._2 - b._2)














  def part2() = {

  }
}
