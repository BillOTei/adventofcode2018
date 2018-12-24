package days

import scala.io.Source

object Day12 {
  private val initState = "#..#.#..##......###...###"
  private val notes = Source
    .fromFile("sources/source12.txt")
    .getLines
    .map(s => {
      val split = s.split(" => ")
      (split(0), split(1))
    })
    .toArray

  def part1() = {
    def go(state: Map[Int, Char], gen: Int): Map[Int, Char] = {
      if (gen == 20) state
      else go(updateState(state), gen + 1)
    }

    go(
      initState.zipWithIndex.map(_.swap).toMap,
      0
    ).toList.sortBy(_._1).map(_._2).mkString("")
  }

  private def updateState(initState: Map[Int, Char]) = {
    val keys = initState.keys
    val (first, last) = (keys.min, keys.max)

    val initStateString = initState.toArray.sortBy(_._1).map(_._2).mkString("")

    val newPlants = notes
      .map(note => {
        s"\\Q${note._1}\\E".r
          .findAllMatchIn("...." + initStateString + "....")
          .map(_.start + 2 - 4)
          .toList
      })
      .filter(_.nonEmpty)
      .reduce((acc, a) => acc ++ a)
      .toArray

    val begin = Array(first, newPlants.min).min
    val end = Array(last, newPlants.max).max

    (begin to end)
      .map(i => {
        if (newPlants.contains(i)) (i, '#')
        else (i, '.')
      })
      .toMap
  }

  def part2() = {}
}
