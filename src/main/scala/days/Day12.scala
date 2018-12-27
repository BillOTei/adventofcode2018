package days

import scala.io.Source

object Day12 {
  private val initState =
    "##..##....#.#.####........##.#.#####.##..#.#..#.#...##.#####.###.##...#....##....#..###.#...#.#.#.#"
  private val notes = Source
    .fromFile("sources/source12.txt")
    .getLines
    .map(s => {
      val split = s.split(" => ")
      (split(0), split(1))
    })
    .toArray

  def part1(): Int = {
    def go(state: Map[Int, Char], gen: Int): Map[Int, Char] = {
      if (gen == 20) state
      else go(updateState(state), gen + 1)
    }

    go(
      initState.zipWithIndex.map(_.swap).toMap,
      0
    ).toArray
      .filter(_._2 == '#')
      .sortBy(_._1)
      .map(_._1)
      .sum
  }

  private def paddedState(initState: Map[Int, Char]) = {
    val keys = initState.keys
    val (first, last) = (keys.min, keys.max)
    val headMap = ((first - 4) until first).map((_, '.')).toMap
    val tailMap = (last + 1 to last + 4).map((_, '.')).toMap

    (headMap ++ initState ++ tailMap).mapValues(_ => '.')
  }

  private def paddedMatches(state: Map[Int, Char]) = {
    val strState = state.toArray.sortBy(_._1).map(_._2).mkString("")
    val padding = 2 - 4 + state.minBy(_._1)._1
    // 2: center of the note, 4: empty pots added, min state idx: the shift along the initial state

    notes
      .map(note => {
        s"""(?=${note._1})""".r
          .findAllMatchIn("...." + strState + "....")
          .map(_.start + padding)
          .toList
      })
      .filter(_.nonEmpty)
      .reduce((acc, a) => acc ++ a)
      .toArray
  }

  private def updateState(initState: Map[Int, Char]) = {
    val newPlants = paddedMatches(initState)

    val check = initState.toArray.sortBy(_._1).map(_._2).mkString("")

    val newState = paddedState(initState)
    val rArray = newPlants
      .foldLeft(newState)((st, i) => st.updated(i, '#'))
      .toArray
      .sortBy(_._1)
    val (start, end) = (
      rArray.find(_._2 == '#').get._1,
      rArray(rArray.lastIndexWhere(_._2 == '#'))._1
    )

    rArray.filter(p => p._1 >= start && p._1 <= end).toMap
  }

  def part2(): Int = {
    def go(state: Map[Int, Char], gen: Long): Map[Int, Char] = {
      if (gen == 50000000000L) state
      else go(updateState(state), gen + 1)
    }

    go(
      initState.zipWithIndex.map(_.swap).toMap,
      0
    ).toArray
      .filter(_._2 == '#')
      .sortBy(_._1)
      .map(_._1)
      .sum
  }
}
