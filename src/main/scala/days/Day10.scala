package days

import scala.io.Source

object Day10 {
  private val reg = """(?:-|)\d+""".r
  private val points = Source
    .fromFile("sources/source10.txt")
    .getLines
    .map(s => {
      val point = reg.findAllMatchIn(s).map(_.matched.trim.toInt).toList

      ((point.head, point(1)), (point(2), point(3)))
    })
    .toVector

  def part1() = {
    val (maxX, minX, maxY, minY) = (points.maxBy(_._1._1)._1._1,
                                    points.minBy(_._1._1)._1._1,
                                    points.maxBy(_._1._2)._1._2,
                                    points.minBy(_._1._2)._1._2)

    val grid = {
      for {
        x <- minX to maxX
        y <- minY to maxY
      } yield (x, y)
    }.groupBy(_._2)
      .toList
      .sortBy(_._1)

    display(grid)
  }

  def part2() = {}

  private def display(l: List[(Int, Seq[(Int, Int)])]): Unit = {
    l.map(_._2.map(p => points.find(_._1 == p).map(_ => "#").getOrElse(".")))
      .foreach(println)
  }
}
