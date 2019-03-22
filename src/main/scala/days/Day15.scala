package days

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.io.Source

object Day15 {
  val fightersArr: ArrayBuffer[((Int, Int), String, Int)] = ArrayBuffer()
  val map: Array[((Int, Int), String, Int)] = Source
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
              fightersArr.+=(((p._2, l._2), p._1, 200))

            ((p._2, l._2), p._1, 200)
          }))
    .toArray
    .sortBy(p => (p._1._2, p._1._1))

  def part1() = {
    val m = map
    val frs = fightersArr.sortBy(f => (f._1._2, f._1._1))

    def go(
            fighters: ArrayBuffer[((Int, Int), String, Int)],
            prevFighters: ArrayBuffer[((Int, Int), String, Int)],
            map: Array[((Int, Int), String, Int)],
            r: Int): ArrayBuffer[((Int, Int), String, Int)] = {

      if (r == 46) {
        val t = 0
      }

      val (newFrs, newMap) = round(
        map,
        fighters,
        0,
        fighters.size
      )

      if (newFrs.sortBy(f => (f._1._2, f._1._1)).toArray.deep == fighters.toArray.deep) {
        fighters
      } else {
        go(newFrs, fighters, newMap, r + 1)
      }
    }

    go(frs, ArrayBuffer.empty, m, 0)
  }

  def part2() = {}

  private def surr(x: Int, y: Int) =
    List((x, y - 1), (x, y + 1), (x - 1, y), (x + 1, y))

  private def fight(
                     directOppo: ArrayBuffer[((Int, Int), String, Int)],
                     m: Array[((Int, Int), String, Int)],
                     fighters: ArrayBuffer[((Int, Int), String, Int)]
                   ) = {
    if (directOppo.nonEmpty) {
      val opponent = directOppo.minBy(opp => (opp._3, opp._1._2, opp._1._1))
      if (opponent._3 - 3 <= 0) {
        (
          m.map(p => if (p._1 == opponent._1) (p._1, ".", 200) else p),
          fighters.filter(_._1 != opponent._1),
        )
      } else {
        (
          m.map(p => if (p._1 == opponent._1) opponent.copy(_3 = opponent._3 - 3) else p),
          fighters.map(fighter => if (fighter._1 == opponent._1) opponent.copy(_3 = opponent._3 - 3) else fighter),
        )
      }
    } else (m, fighters)
  }

  private def round(m: Array[((Int, Int), String, Int)],
                    fighters: ArrayBuffer[((Int, Int), String, Int)],
                    i: Int,
                    len: Int): (ArrayBuffer[((Int, Int), String, Int)], Array[((Int, Int), String, Int)]) = {
    if (i >= len) {
      return (fighters, m)
    }

    val f = fighters(i)
    val directOppo = directOpponents(fighters.filter(_ != f), f)
    if (directOppo.nonEmpty) {
      // Direct fight
      val (newDirectMap, newDirectFighters) = fight(directOppo, m, fighters)

      return round(
        newDirectMap,
        newDirectFighters,
        i + 1 - (len - newDirectFighters.size),
        newDirectFighters.size
      )
    }

    val opponents = fighters.filter(_._2 != f._2)
    val inRangeReachable = opponents
      .flatMap(o => getInRange(m, o))
      .foldLeft(Array[Array[((Int, Int), String, Int)]]())((acc, p) => {
        val path = bfs(m, f, p)
        if (path.nonEmpty) {
          acc :+ path
        } else acc
      })
      .sortBy(_.length)
    val firstStep = inRangeReachable.headOption.map(_ (1))
    if (firstStep.isEmpty) {
      return round(m, fighters, i + 1, len)
    }

    val movedFighter = (firstStep.get._1, f._2, f._3)
    val (newMap, newFighters) = fight(
      directOpponents(fighters.filter(_ != f), movedFighter),
      m,
      fighters
    )

    round(
      newMap.map(p => {
        if (p._1 == firstStep.get._1) movedFighter
        else if (p._1 == f._1) (p._1, ".", 200)
        else p
      }),
      newFighters.map(fi => if (fi == f) movedFighter else fi),
      i + 1 - (len - newFighters.size),
      newFighters.size
    )
  }

  private def directOpponents(opponents: ArrayBuffer[((Int, Int), String, Int)],
                              f: ((Int, Int), String, Int)) = {
    opponents.filter(o => {
      val (x, y) = f._1
      surr(x, y).contains(o._1) && o._2 != f._2
    })
  }

  private def bfs(map: Array[((Int, Int), String, Int)],
                  start: ((Int, Int), String, Int),
                  end: ((Int, Int), String, Int)) = {

    def solve(s: ((Int, Int), String, Int)) = {
      val q = mutable.Queue[((Int, Int), String, Int)]()
      q.enqueue(s)

      val visited = collection.mutable.Map(map.map(_ -> false): _*)
      visited(s) = true

      val prev = collection.mutable
        .Map[((Int, Int), String, Int), Option[((Int, Int), String, Int)]](
        map.map(_ -> None): _*
      )

      while (q.nonEmpty) {
        val node = q.dequeue()

        val surroundings = map.filter(p => {
          val (x, y) = node._1
          surr(x, y).contains(p._1) && p._2 == "."
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
                         start: ((Int, Int), String, Int),
                         end: ((Int, Int), String, Int),
                         p: collection.mutable.Map[((Int, Int), String, Int),
                           Option[((Int, Int), String, Int)]]) = {
      def go(path: Array[((Int, Int), String, Int)],
             n: ((Int, Int), String, Int)): Array[((Int, Int), String, Int)] = {
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

  private def getInRange(map: Array[((Int, Int), String, Int)],
                         p: ((Int, Int), String, Int)) = {
    val (x, y) = p._1
    val surroundings = surr(x, y)

    map
      .filter(pt => surroundings.contains(pt._1) && pt._2 == ".")
      .sortBy(p => (p._1._2, p._1._1))
  }
}
