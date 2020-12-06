package adv

object DayOne extends BaseApp {

  def filename = "input"

  def partOne(): Unit = {
    val first = lines.map(_.toInt).sorted
    val second = first

    for (f <- first) {
      for (s <- second) {
        if (f + s == 2020) {
          println(f * s)
          return
        }
      }
    }
  }

  def partTwo(): Unit = {
    val first = lines.map(_.toInt).sorted
    val second = first
    val third = first

    for (f <- first) {
      for (s <- second) {
        for (t <- third) {
          if (f + s + t == 2020) {
            println(f * s * t)
            return
          }
        }
      }
    }
  }
}