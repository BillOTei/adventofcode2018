package days

object Day11 {
  private val gridSerialNb = 4151
  private val mainGrid = for {
    x <- 1 to 300
    y <- 1 to 300
  } yield ((x, y), getPower(x, y))

  private def grids(size: Int) = {
    val gridsNb = (300 - size + 1) * (300 - size + 1)
    var gridsCount = 0
    println(size)
    mainGrid.takeWhile(_ => gridsCount <= gridsNb).map {
      case ((x, y), _) =>
        val limit = size - 1
        if (x + limit <= 300 && y + limit <= 300) {
          val grid = for {
            xGrid <- x to x + limit
            yGrid <- y to y + limit
          } yield getPower(xGrid, yGrid)

          gridsCount += 1

          (size, grid(0)._1, grid.map(_._2).sum)
        } else {
          (0, (0, 0), 0)
        }
    }
  }.filter(_._2 != (0, 0))

  private def getPower(x: Int, y: Int) = {
    val z = (((x + 10) * y + gridSerialNb) * (x + 10)).toString
    if (z.length < 3) ((x, y), -5)
    else ((x, y), z.takeRight(3).head.asDigit - 5)
  }

  def part1(): (Int, (Int, Int), Int) = {
    grids(100).maxBy(_._3)
  }

  def part2(): (Int, (Int, Int), Int) = {
    (1 to 300).flatMap(grids).maxBy(_._3)
  }
}
