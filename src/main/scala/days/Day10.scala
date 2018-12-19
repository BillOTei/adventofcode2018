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

  def solve(): Unit = {
    val (maxX, minX, maxY, minY) = boundaries(points)

    go(points, Math.abs((maxX - minX) * (maxY - minY)), 0)
  }

  private def go(points: Vector[((Int, Int), (Int, Int))], area: Int, counter: Int): Unit = {
    val (maxX, minX, maxY, minY) = boundaries(points)

    val a = Math.abs((maxX - minX) * (maxY - minY))
    if (a <= area) {
      if (a <= 1000) {
        val grid = {
          for {
            x <- minX to maxX
            y <- minY to maxY
          } yield (x, y)
        }.groupBy(_._2)
          .toList
          .sortBy(_._1)

        display(grid, points)

        println(counter)
      }

      go(move(points), a, counter + 1)
    } else {
      go(move(points), area, counter + 1)
    }
  }

  private def display(l: List[(Int, Seq[(Int, Int)])],
                      points: Vector[((Int, Int), (Int, Int))]): Unit = {
    l.map(_._2.map(p => points.find(_._1 == p).map(_ => "#").getOrElse(".")))
      .foreach(println)
  }

  private def move(points: Vector[((Int, Int), (Int, Int))]) = {
    points.map(p => {
      ((p._1._1 + p._2._1, p._1._2 + p._2._2), p._2)
    })
  }

  private def boundaries(points: Vector[((Int, Int), (Int, Int))]) = {
    (points.maxBy(_._1._1)._1._1,
     points.minBy(_._1._1)._1._1,
     points.maxBy(_._1._2)._1._2,
     points.minBy(_._1._2)._1._2)
  }
}
