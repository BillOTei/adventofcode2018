package days

import scala.collection.mutable.ArrayBuffer
import scala.io.Source

object Day15 {
  val fightersArr: ArrayBuffer[((Int, Int), String)] = ArrayBuffer()
  val map: Array[((Int, Int), String)] = Source.fromFile("sources/source15.txt").getLines
    .zipWithIndex
    .flatMap(l => l._1
      .split("")
      .zipWithIndex
      .map(p => {
        if (List("G", "E").contains(p._1)) fightersArr.+=(((p._2, l._2), p._1))

        ((p._2, l._2), p._1)
      })
    )
    .toArray

  def part1() = {
    val m = map
    val frs = fightersArr.sortBy(f => (f._1._2, f._1._1))

    def go(fighters: ArrayBuffer[((Int, Int), String)], i: Int) = {
      val f = fighters(i)
      val opponents = fighters.filter(_._2 != f._2)

    }
  }

  def part2() = {

  }
}
