package adv

object DayTwo extends BaseApp {
  def filename = "input_day2"

  def partOne(lines: List[String]): Int = {
    def checkPassword(rule: String): Boolean = {
      val separators = Array(':', ' ', '-')

      rule.split(separators).toList match {
        case min :: max :: letter :: _ :: pwd :: _ =>
          val occurrences = pwd.toCharArray.groupBy(identity).map {
            case (char, list) => char -> list.length
          }
          val count = occurrences.getOrElse(letter.toCharArray.head, 0)
          count >= min.toInt && count <= max.toInt
        case Nil => false
      }
    }

    lines.map(checkPassword).count(_ == true)
  }

  def partTwo(lines: List[String]): Int = {
    def checkPassword(rule: String): Boolean = {
      val separators = Array(':', ' ', '-')

      rule.split(separators).toList match {
        case min :: max :: letter :: _ :: pwd :: _ =>
          val minIndex = min.toInt - 1
          val maxIndex = max.toInt - 1
          val first = pwd.lift(minIndex).getOrElse(" ").toString
          val second = pwd.lift(maxIndex).getOrElse(" ").toString
          second != first && (first == letter || second == letter)
        case _ => false
      }
    }

    lines.map(checkPassword).count(_ == true)
  }

  println(partOne(lines))
  println(partTwo(lines))
}
