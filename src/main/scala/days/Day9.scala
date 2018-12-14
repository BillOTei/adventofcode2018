package days

object Day9 {
  private val players = 10

  def part1() = {
    val playersMap = (0 until players).map(_ -> 0).toMap
    val marblesMap = scala.collection.mutable.Map[Int, Int](0 -> 0)

    var play = true

    Stream.from(1).takeWhile(marble => {
      if (marble % 23 == 0) {

      } else {
        val pos = marblesMap.find(_._2 == marble - 1).get._1
        val last = marblesMap.last._1
        if (pos == last)
      }

      play
    })
  }

  def part2() = {
  }
}
