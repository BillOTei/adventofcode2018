package days

import scala.io.Source

object Day5 {
  private lazy val polymer = Source.fromFile("sources/source5.txt").getLines().toList.head.toList

  def part1(): Int = {
    collapsePolymer(polymer).length
  }

  private def collapsePolymer(p: List[Char]) = p.foldLeft("")((acc, char) => {
    if (!acc.isEmpty && acc.last != char && acc.last.toLower == char.toLower) {
      acc.dropRight(1)
    } else acc + char
  })

  def part2() = {

  }
}
