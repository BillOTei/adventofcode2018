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
        .filter(p => {
          val t = bfs(m, f, p)

          true
        })

      1
    }

    go(frs, 0)
  }

  private def bfs(map: Array[((Int, Int), String)],
                  start: ((Int, Int), String),
                  end: ((Int, Int), String)) = {
    def go(q: Array[((Int, Int), String)],
           m: Array[((Int, Int), String)]): Array[((Int, Int), String)] = {

      val remaining = ArrayBuffer[((Int, Int), String)]()
      val surroundings = q.flatMap(node => {
        val (s, r) = m.partition(p => {
          val (x, y) = node._1
          List((x, y - 1), (x, y + 1), (x + 1, y), (x - 1, y))
            .contains(p._1) && p._2 == "."
        })

        remaining.++=(r)

        s
      })

      if (surroundings.contains(end)) {
        q ++ surroundings
      } else {
        go(q ++ surroundings, remaining.toArray)
      }
    }

    go(Array(start), map)
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
