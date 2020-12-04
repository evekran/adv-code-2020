package adv

object DayTwo extends BaseApp {
  def filename = "input_day2"

  def partOne(lines: List[String]): Int = {
    def checkPassword(rule: String): Boolean = {
      val separators = Array(':', ' ', '-')

      rule.split(separators).toList match {
        case min :: max :: letter :: _ :: pwd :: _ =>
          val occ = pwd.toCharArray.groupBy(identity).map {
            case (char, list) => char -> list.length
          }
          val count = occ.getOrElse(letter.toCharArray.head, 0)
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
          val first = pwd.lift(min.toInt - 1).getOrElse(" ")
          val second = pwd.lift(max.toInt - 1).getOrElse(" ")
          second != first && (first.toString == letter || second.toString == letter)
        case Nil => false
      }
    }

    lines.map(checkPassword).count(_ == true)
  }

  println(partOne(lines))
  println(partTwo(lines))
}
