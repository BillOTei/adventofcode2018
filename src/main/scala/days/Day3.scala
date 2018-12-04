package days

import scala.io.Source

object Day3 {
  def part1(): Int = {
    Source.fromFile("sources/source3.txt").getLines.toVector
      .flatMap(getRectangle)
      .groupBy(identity)
      .count(_._2.length > 1)
  }

  private def getRectangle(l: String) = {
    val data = l.split("""\D+""").tail
//    val id = data(0).toInt
    val xStart = data(1).toInt
    val yStart = data(2).toInt
    val xLen = data(3).toInt
    val yLen = data(4).toInt

    for {
      i <- xStart until xStart + xLen
      j <- yStart until yStart + yLen
    } yield (i, j)
  }

  def part2() = {
    ???
  }
}
