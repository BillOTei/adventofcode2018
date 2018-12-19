package days

object Day9 {
  private val players = 411
  private val marbles = 71058
  private val playersMap = {
    val m = scala.collection.mutable.Map[Int, Int]()
    (1 to players).map(_ -> 0).foreach(p => m(p._1) = 0)

    m
  }

  def part1(): Int = {
    go(Vector(0, 2, 1), 3, 1, 3, marbles)
  }

  def go(l: Vector[Int],
         marble: Int,
         iCurrMarble: Int,
         currPlayer: Int,
         points: Int): Int = {
    val player = if (currPlayer == players) 1 else currPlayer + 1
    val lastIdx = l.length - 1
    val nextIdx = iCurrMarble + 2

    if (marble > points) {
      playersMap.maxBy(_._2)._2
    } else if (marble % 23 == 0) {
      val idxToRemove =
        if (iCurrMarble - 7 >= 0) iCurrMarble - 7
        else l.length - Math.abs(iCurrMarble - 7)

      playersMap(currPlayer) = playersMap(currPlayer) + marble + l(idxToRemove)

      val newL = l.slice(0, nextIdx) ++: l.slice(nextIdx, lastIdx + 1)

      go(
        newL.slice(0, idxToRemove) ++: newL.slice(idxToRemove + 1, newL.length),
        marble + 1,
        idxToRemove,
        player,
        points)
    } else {
      if (nextIdx == lastIdx + 1) {
        go(l :+ marble, marble + 1, nextIdx, player, points)
      } else if (nextIdx > lastIdx) {
        go(Vector(l(0), marble) ++: l.slice(1, lastIdx + 1),
          marble + 1,
          1,
          player,
          points)
      } else {
        go(l.slice(0, nextIdx) ++: Vector(marble) ++: l.slice(nextIdx,
          lastIdx + 1),
          marble + 1,
          nextIdx,
          player,
          points)
      }
    }
  }

  def part2(): Int = {
    go(Vector(0, 2, 1), 3, 1, 3, marbles * 100)
  }
}
