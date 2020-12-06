package adv

object DayThree extends BaseApp {
  def filename = "input_day3"

  val mapWidth = lines.head.length

  def slopeCheck(stepX: Int, stepY: Int): Int = {
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

  override def partOne(): Unit = {
    val result = slopeCheck(3, 1)

    println(result)
  }

  override def partTwo(): Unit = {
    val slOne = slopeCheck(1, 1)
    val slTwo = slopeCheck(3, 1)
    val slThree = slopeCheck(5, 1)
    val slFour = slopeCheck(7, 1)
    val slFive = slopeCheck(1, 2)

    val result = slOne * slTwo * slThree * slFour * slFive

    println(result)
  }
}
