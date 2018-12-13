package days

import scala.io.Source

object Day8 {
  private lazy val source = Source.fromFile("sources/source8.txt").getLines.toList
    .head
    .split(" ")
    .map(_.toInt)
    .toList

  def part1(): Int = {
    def go(l: List[Int], acc: Int, currMetaCount: Map[Int, Int], currChildrenCount: Map[Int, Int]): Int = {
      if (l.isEmpty) {
        return acc
      } else if (currMetaCount.size == 1 && l.length == currMetaCount.head._2) {
        return acc + l.sum
      }

      val lastChildrenCount = currChildrenCount.last._2
      val lastChildrenCountIdx = currChildrenCount.keys.last

      val childrenCount = l.head
      val metaCount = l(1)

      if (childrenCount > 0) {
        if (lastChildrenCount > 0) {
          val lastMapIdx = currMetaCount.keys.toList.max
          val newCurrChildrenCount = currChildrenCount.updated(lastChildrenCountIdx, lastChildrenCount - 1)

          go(
            l.tail.drop(1),
            acc, currMetaCount + ((lastMapIdx + 1) -> metaCount),
            newCurrChildrenCount + ((lastChildrenCountIdx + 1) -> childrenCount)
          )
        } else {
          go(l.tail.drop(1), acc, Map(0 -> metaCount), Map(0 -> childrenCount))
        }
      } else {
        if (lastChildrenCount > 1) {
          val newCurrChildrenCount = currChildrenCount.updated(lastChildrenCountIdx, lastChildrenCount - 1)

          go(
            l.tail.drop(metaCount + 1),
            acc + l.tail.slice(1, metaCount + 1).sum,
            currMetaCount,
            newCurrChildrenCount
          )
        } else {
          val lastMeta = currMetaCount.last
          val lastMetaCount = lastMeta._2
          val lastMetaSum = l.tail.slice(metaCount + 1, lastMetaCount + metaCount + 1).sum

          go(
            l.tail.drop(lastMetaCount + metaCount + 1),
            acc + l.tail.slice(1, metaCount + 1).sum + lastMetaSum,
            if ((currMetaCount - lastMeta._1).isEmpty) Map(0 -> 0) else currMetaCount - lastMeta._1,
            if ((currChildrenCount - lastChildrenCountIdx).isEmpty) Map(0 -> 0) else currChildrenCount - lastChildrenCountIdx
          )
        }
      }
    }

    go(source, 0, Map(0 -> 0), Map(0 -> 0))
  }


  def part2(): Int = {
    ???
  }
}
