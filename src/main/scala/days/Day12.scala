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

    notes
      .map(note => {
        s"\\Q${note._1}\\E".r.findAllMatchIn(initState).map(_.start - 3).toList
      })
      .foreach(println)
  }

  def part2() = {}
}
