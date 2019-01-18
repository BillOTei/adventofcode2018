package days

import scala.collection.mutable.ArrayBuffer
import scala.io.Source

object Day15 {
  val fightersArr: ArrayBuffer[((Int, Int), String)] = ArrayBuffer()
  val map: Array[((Int, Int), String)] = Source
    .fromFile("sources/source15.txt")
    .getLines
    .zipWithIndex
    .flatMap(
      l =>
        l._1
          .split("")
          .zipWithIndex
          .map(p => {
            if (List("G", "E").contains(p._1))
              fightersArr.+=(((p._2, l._2), p._1))

            ((p._2, l._2), p._1)
          }))
    .toArray

  def part1() = {
    val m = map
    val frs = fightersArr.sortBy(f => (f._1._2, f._1._1))

    def go(fighters: ArrayBuffer[((Int, Int), String)], i: Int) = {
      val f = fighters(i)
      val opponents = fighters.filter(_._2 != f._2)
      val inRangeReachable = opponents
        .flatMap(o => getInRange(m, o))
        .foldLeft(List[(((Int, Int), String), Int)]())((acc, p) => {
          val count = bfs(m, f, p)

          if (count != -1) {
            acc :+ (p, count)
          } else acc
        })
        .sortBy(p => (p._2, p._1._1._2, p._1._1._1)) // Sort by path length then reading order (y then x)

      inRangeReachable.head
    }

    go(frs, 0)
  }

  private def bfs(map: Array[((Int, Int), String)],
                  start: ((Int, Int), String),
                  end: ((Int, Int), String)): Int = {

    val freeCount = map.count(_._2 == ".")

    def go(q: Array[((Int, Int), String)],
           m: Array[((Int, Int), String)],
           v: ArrayBuffer[((Int, Int), String)],
           i: Int): Int = {
      val remaining = ArrayBuffer[((Int, Int), String)]()
      val localVisited = ArrayBuffer[((Int, Int), String)]()

      val surroundings = q
        .filterNot(n => v.contains(n))
        .flatMap(node => {
          val (s, r) = m.partition(p => {
            val (x, y) = node._1
            List((x, y - 1), (x, y + 1), (x + 1, y), (x - 1, y))
              .contains(p._1) && p._2 == "."
          })

          remaining.++=(r)
          localVisited.+=(node)

          s
        })

      if (surroundings.contains(end)) {
        i + 1
      } else if (v.size + localVisited.size == freeCount - 1) {
        -1
      } else {
        go((q ++ surroundings).distinct,
           remaining.toArray,
           v.++=(localVisited).distinct,
           i + 1)
      }
    }

    go(Array(start), map, ArrayBuffer.empty, 0)
  }

  private def getInRange(map: Array[((Int, Int), String)],
                         p: ((Int, Int), String)) = {
    val (x, y) = p._1
    val surroundings = List((x, y - 1), (x, y + 1), (x + 1, y), (x - 1, y))

    map
      .filter(pt => surroundings.contains(pt._1) && pt._2 == ".")
      .sortBy(p => (p._1._2, p._1._1))
  }

  def part2() = {}
}
