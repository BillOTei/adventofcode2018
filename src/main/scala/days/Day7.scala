package days

import scala.io.Source

object Day7 {
  private lazy val list = Source.fromFile("sources/source7.txt").getLines()
  private val regex = "Step ([A-Z]) must be finished before step ([A-Z]) can begin.".r
  private lazy val edges = list.map {
    case regex(a, b) => Edge(a.head, b.head)
  }.toSeq
  private lazy val initVertices = edges.flatMap(e => Seq(e.from, e.to)).filter(c => edges.forall(_.to != c)).toSet
  case class Edge(from: Char, to: Char)

  def part1(): String = {
    sort(edges, initVertices, "")
  }

  def part2(): Long = {
    reduce(edges.toList.map(e => (e.from, e.to)), char => 61 + (char - 'A'), 5)
  }

  def sort(edges: Seq[Edge], set: Set[Char], acc: String): String = {
    if(set.nonEmpty) {
      val n = set.minBy(identity)

      val (newEdges, newSet) = edges.filter(_.from == n).foldLeft((edges, set.filter(_ != n))) {
        case ((tEdges, tSet), edge) =>
          val filtered = tEdges.filter(_ != edge)
          (filtered, if(filtered.forall(_.to != edge.to)) tSet + edge.to else tSet)
      }

      sort(newEdges, newSet, acc + n)
    } else
      acc
  }

  def reduce(input: List[(Char, Char)], charTime: Char => Long, concurrent: Int): Long = {
    def go(steps: List[(Char, Char)], solving: List[(Long, Char)], time: Long): Long = {
      steps match {
        case Nil => time
        case list =>
          val keys = list.map(_._1).toSet
          val values = list.map(_._2).toSet
          val diffs = keys.diff(values).toList
            .filter(c => !solving.map(_._2).contains(c))
          val times = diffs.map(c => (time + charTime(c)) → c)

          val neew = (solving ++ times.sortBy(_._1).take(concurrent - solving.length)).sortBy(_._1)
          neew match {
            case Nil => time
            case (t, c) :: tail =>
              val tuples = list.filter {
                case (c1, _) => c1 != c
              }
              if (tuples.nonEmpty) go(tuples, tail, t)
              else t + list.map(_._2).map(charTime).max
          }
      }
    }

    go(input, List.empty, 0)
  }
}
