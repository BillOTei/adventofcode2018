package days

import scala.collection.mutable
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

          acc
//          if (count != -1) {
//            acc :+ (p, count)
//          } else acc
        })
        .sortBy(p => (p._2, p._1._1._2, p._1._1._1)) // Sort by path length then reading order (y then x)

      inRangeReachable.head
    }

    go(frs, 0)
  }

  private def bfs(map: Array[((Int, Int), String)],
                  start: ((Int, Int), String),
                  end: ((Int, Int), String)) = {

    def solve(s: ((Int, Int), String)) = {
      val q = mutable.Queue[((Int, Int), String)]()
      q.enqueue(s)

      val visited = collection.mutable.Map(map.map(_ -> false): _*)
      visited(s) = true

      val prev = collection.mutable
        .Map[((Int, Int), String), Option[((Int, Int), String)]](
          map.map(_ -> None): _*
        )

      while (q.nonEmpty) {
        val node = q.dequeue()

        val surroundings = map.filter(p => {
          val (x, y) = node._1
          List((x, y - 1), (x, y + 1), (x + 1, y), (x - 1, y))
            .contains(p._1) && p._2 == "."
        })

        surroundings.foreach(surr => {
          if (!visited(surr)) {
            q.enqueue(surr)
            visited(surr) = true
            prev(surr) = Some(node)
          }
        })
      }

      prev
    }

    def reconstructPath(
        start: ((Int, Int), String),
        end: ((Int, Int), String),
        p: collection.mutable.Map[((Int, Int), String),
                                  Option[((Int, Int), String)]]) = {
      def go(path: Array[((Int, Int), String)],
             n: ((Int, Int), String)): Array[((Int, Int), String)] = {
        val parent = p(n)
        if (path.contains(start)) {
          path.reverse :+ end
        } else if (parent.isEmpty) {
          Array.empty
        } else {
          go(path :+ parent.get, parent.get)
        }
      }

      go(Array.empty, end)
    }

    val prev = solve(start)

    reconstructPath(start, end, prev)
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
