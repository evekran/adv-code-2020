package adv

object DayThree extends BaseApp {
  def filename = "input_day3"

  val patternL = lines.head.length

  private def slope_check(inc_x: Int, inc_y: Int) = {
    var c = 0
    var x = 0
    var y = 0

    while (y < lines.length) {
      x += inc_x
      y += inc_y
      if (x >= patternL) {
        x = x - patternL
      }

      if (y < lines.length) {
        val p = lines(y)(x)
        print(p)

        if (p.toString == "#") {
          c += 1
        }
      }

    }

    c
  }

  val slOne = slope_check(1, 1)
  val slTwo = slope_check(3, 1)
  val slThree = slope_check(5, 1)
  val slFour = slope_check(7, 1)
  val slFive = slope_check(1, 2)

  print(slOne * slTwo * slThree * slFour * slFive)

}
