package days

import org.joda.time.DateTime
import org.joda.time.format.DateTimeFormat

import scala.io.Source

object Day4 {
  def part1() = {
    val records = Source.fromFile("sources/source4.txt").getLines.toVector
      .map(parseData)
      .sortWith(_.dateTime.getMillis < _.dateTime.getMillis)
    val len = records.length
    val firstGuard = records.head.guardId.get
    val allGuards = records.filter(_.guardId.isDefined).map(_.guardId.get -> 0).toMap

    def go(guards: Map[Int, Int], i: Int, currGuard: Int) = {
      if (i == len - 1) {

      }
      val record = records(i)
      if (record.guardId.isDefined) go(guards, i + 1, record.guardId.get)
      else if (record.sleeping.get) go(guards.updated(currGuard, records(i + 1)), i + 2, currGuard)
    }
  }

  final case class Record(guardId: Option[Int], dateTime: DateTime, sleeping: Option[Boolean])

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

  def part2() = {
    val source = Source.fromFile("sources/source4.txt").getLines.toVector

  }
}
