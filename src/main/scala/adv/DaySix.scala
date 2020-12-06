package adv

object DaySix extends BaseApp {
  override def filename: String = "input_day6"

  def getGroups: List[String] = {
    val groupSeparator = "@"

    lines.map {
      case inputRow if inputRow == "" => groupSeparator
      case x => x
    }.mkString(" ").split(groupSeparator).toList.map(_.trim)
  }

  override def partOne(): Unit = {
    val groups = getGroups
    val answers = groups.map(_.split("").toList.distinct.count(_ != " ")).sum

    println(answers)
  }

  override def partTwo(): Unit = {
    val groups = getGroups
    val groupAnswers = groups.map(_.split(" ").toList)

    val result = groupAnswers.map { group =>
      group.map(_.split("").toSet[String]).reduce(_ intersect _).size
    }.sum

    println(result)
  }
}
