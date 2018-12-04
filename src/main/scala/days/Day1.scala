package days

import scala.io.Source

object Day1 {
  def part1(): Long = {
    Source.fromFile("sources/source1.txt").getLines.foldLeft(0.toLong)((acc, i) => acc + i.toLong)
  }

  def part2(): Long = {
    val source = Source.fromFile("sources/source1.txt").getLines.toList
    val l = source.length

    def go(acc: Map[Int, Int], i: Int, counter: Int): Int = {
      if (i > l - 1) {
        go(acc, 0, counter)
      } else if (acc.get(counter + source(i).toInt).isDefined) {
        counter + source(i).toInt
      } else {
        go(acc + (counter + source(i).toInt -> 0), i + 1, counter + source(i).toInt)
      }
    }

    go(Map(0 -> 0), 0, 0)
  }
}
