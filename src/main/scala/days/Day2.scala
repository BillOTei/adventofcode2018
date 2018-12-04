package days

import scala.io.Source

object Day2 {
  def part1(): Int = {
    val r = Source.fromFile("sources/source2.txt").getLines.map(
      _.groupBy(identity)
        .foldLeft(0, 0)((acc, count) => {
          if (count._2.length == 2 && acc._1 == 0) (1, acc._2)
          else if (count._2.length == 3 && acc._2 == 0) (acc._1, 1)
          else acc
        })
    ).foldLeft((0, 0)) { case ((accA, accB), (a, b)) => (accA + a, accB + b) }

    r._1 * r._2
  }

  def part2(): Long = {
    ???
  }
}
