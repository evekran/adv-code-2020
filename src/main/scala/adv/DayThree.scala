package adv

object DayThree extends BaseApp {
  def filename = "input_day3"

  val mapWidth = lines.head.length

  private def slope_check(stepX: Int, stepY: Int): Int = {
    var treeCount = 0
    var posX = 0
    var posY = 0

    while (posY < lines.length) {
      posX += stepX
      posY += stepY
      if (posX >= mapWidth) {
        posX = posX - mapWidth
      }

      if (posY < lines.length) {
        val point = lines(posY)(posX)
        if (point.toString == "#") {
          treeCount += 1
        }
      }

    }

    treeCount
  }

  val slOne = slope_check(1, 1)
  val slTwo = slope_check(3, 1)
  val slThree = slope_check(5, 1)
  val slFour = slope_check(7, 1)
  val slFive = slope_check(1, 2)

  print(slOne * slTwo * slThree * slFour * slFive)

}
