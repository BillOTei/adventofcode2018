package days

import scala.collection.mutable.ArrayBuffer
import scala.io.Source

object Day13 {
  private val carts =
    ArrayBuffer[(Int, Int, String, (String, String, String))]()

  private def source(): Array[Array[(Int, Int, String)]] = {
    Source
      .fromFile("sources/source13.txt")
      .getLines
      .zipWithIndex
      .map(
        l =>
          l._1
            .split("")
            .zipWithIndex
            .filter(_._1 != " ")
            .map(p => {
              if (List("<", ">", "^", "v").contains(p._1)) {
                carts.+=((p._2, l._2, p._1, ("l", "s", "r")))
              }

              (p._2, l._2, p._1)
            }))
      .toArray
  }
  //.foreach(l => l.foreach(println))

  def part1() = {
    val s = source()
    def go (source: Array[Array[(Int, Int, String)]]): Unit = {
      carts.map(c => {
        val nextStep = source(c._2)
      })

      go(
        source
      )
    }
  }

  def part2() = {}
}
