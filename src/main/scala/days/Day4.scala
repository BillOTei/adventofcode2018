package days

import org.joda.time.DateTime
import org.joda.time.format.DateTimeFormat

import scala.io.Source

object Day4 {
  private lazy val sortedRecords = Source.fromFile("sources/source4.txt").getLines.toVector
    .map(parseData)
    .sortWith(_.dateTime.getMillis < _.dateTime.getMillis)

  def part1(): Int = {
    val r = getGuardsMinuteMap
    val bigSleeper = r.mapValues(_.foldLeft(0)((acc, mn) => acc + mn._2)).maxBy(_._2)._1
    val sleepyMn = r(bigSleeper).maxBy(_._2)._1

    bigSleeper * sleepyMn
  }

  def part2(): Int = {
    val bigSleeper = getGuardsMinuteMap
      .mapValues(_.toVector.sortWith(_._2 > _._2))
      .mapValues(_.maxBy(_._2))
      .maxBy(_._2._2)

    bigSleeper._1 * bigSleeper._2._1
  }

  private def getGuardsMinuteMap: Map[Int, Map[Int, Int]] = {
    val records = sortedRecords
    val len = records.length

    val minutesMap = (0 to 59).map(_ -> 0).toMap
    val firstGuard = records.head.guardId.get
    val allGuards = records.filter(_.guardId.isDefined).map(_.guardId.get -> minutesMap).toMap

    def go(guards: Map[Int, Map[Int, Int]], i: Int, currGuard: Int): Map[Int, Map[Int, Int]] = {
      if (i == len) {
        return guards
      }

      val record = records(i)

      if (record.guardId.isDefined) {
        go(guards, i + 1, record.guardId.get)
      } else if (record.sleeping.get) {
        val range = record.dateTime.getMinuteOfHour until records(i + 1).dateTime.getMinuteOfHour

        go(
          guards.updated(
            currGuard,
            guards(currGuard).map(mn => {
              if (range.contains(mn._1)) mn._1 -> (mn._2 + 1)
              else mn
            })
          ),
          i + 2,
          currGuard
        )
      } else {
        go(guards, i + 1, currGuard)
      }
    }

    go(allGuards, 1, firstGuard)
  }

  private def parseData(l: String) = {
    val dateRegex = "(\\d{4}-\\d{2}-\\d{2} \\d{2}:\\d{2})".r
    val formatter = DateTimeFormat.forPattern("yyyy-MM-dd HH:mm")
    val date = DateTime.parse(dateRegex.findFirstIn(l).get, formatter)

    if (l.contains("falls asleep")) {
      Record(None, date, Some(true))
    } else if (l.contains("wakes up")) {
      Record(None, date, Some(false))
    } else {
      Record(Some(l.split("""\D+""").last.toInt), date, None)
    }
  }

  final case class Record(guardId: Option[Int], dateTime: DateTime, sleeping: Option[Boolean])

}
