package days

object Day14 {
  private val recipesNb = 919901

  def part1() = {
    def go(state: String, p1: Int, p2: Int): String = {
      val (r1, r2) = (state(p1).asDigit, state(p2).asDigit)
      val newState = state + (r1 + r2)
      val l = newState.length

      if (l - 1 >= recipesNb + 10) {
        newState.slice(recipesNb, recipesNb + 10)
      } else {
        go(
          newState,
          if ((r1 + 1) % l + p1 >= l) ((r1 + 1) % l + p1) % l
          else (r1 + 1) % l + p1,
          if ((r2 + 1) % l + p2 >= l) ((r2 + 1) % l + p2) % l
          else (r2 + 1) % l + p2
        )
      }
    }

    go("37", 0, 1)
  }

  def time[R](step: String)(block: => R): R = {
    val t0 = System.nanoTime()
    val result = block
    val t1 = System.nanoTime()
    println(s"Elapsed time $step: " + (t1 - t0) + "ns")
    result
  }

  def part2() = {
    def go(state: String, p1: Int, p2: Int): Int = {
      val (r1, r2) = (state(p1).asDigit, state(p2).asDigit)
      val newState = state + (r1 + r2)
      val l = newState.length
      val recipesNbStr = recipesNb.toString

      if (newState.takeRight(recipesNbStr.length * 2).contains(recipesNbStr)) {
        newState.slice(0, newState.indexOf(recipesNbStr)).length
      } else {
        go(
          newState,
          if ((r1 + 1) % l + p1 >= l) ((r1 + 1) % l + p1) % l
          else (r1 + 1) % l + p1,
          if ((r2 + 1) % l + p2 >= l) ((r2 + 1) % l + p2) % l
          else (r2 + 1) % l + p2
        )
      }
    }

    go("37", 0, 1)
  }
}
