package days

import scala.io.Source

object Day8 {
  private lazy val source = Source.fromFile("sources/source8.txt").getLines.toList
    .head
    .split(" ")
    .map(_.toInt)
    .toList

  case class Node(meta: List[Int], children: List[Node])

  def part1(): Int = {
    def go(l: List[Int], acc: Int, metaEndAcc: Int): Int = {
      val childrenCount = l.head
      val metaCount = l(1)

      if (l.length == metaEndAcc) {
        acc + l.sum
      } else if (childrenCount > 0) {
        go(l.tail.drop(1), acc, metaEndAcc + metaCount)
      } else {
        go(l.tail.drop(metaCount + 1), acc + l.tail.slice(1, metaCount + 1).sum, metaEndAcc)
      }
    }

    go(source, 0, 0)
  }

  def part2() = {

  }
}
