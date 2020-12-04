package adv

object DayFour extends BaseApp {
  def filename = "input_day4"

  val requiredFields = List(
    "byr",
    "iyr",
    "eyr",
    "hgt",
    "hcl",
    "ecl",
    "pid",
  )

  def extractCredentials(input: List[String]): List[String] = {
    input.map {
      case x if x == "" => "@"
      case x => x
    }.mkString(" ").split("@").toList
  }

  def credentialsToMap(credentials: String): Map[String, String] = {
    credentials.split(" ")
      .toList
      .filter(_.nonEmpty)
      .map { attribute =>
        attribute.split(":").toList match {
          case key :: value :: Nil => key -> value
        }
      }.toMap
  }

  def getCredentialsWithRequiredFields(input: List[String]) = {
    extractCredentials(input).map(credentialsToMap).filter { s =>
      requiredFields.forall(rf => s.keys.toList.contains(rf))
    }
  }

  def partOne(input: List[String]): Unit = {
    val result = getCredentialsWithRequiredFields(input)

    println(result.length)
  }

  def partTwo(input: List[String]): Unit = {
    val credentialsMaps = getCredentialsWithRequiredFields(input)

    val result = credentialsMaps.map(cr => {
      val byr = cr.getOrElse("byr", "0").toInt
      val iyr = cr.getOrElse("iyr", "0").toInt
      val eyr = cr.getOrElse("eyr", "0").toInt

      val hgt = cr.getOrElse("hgt", "0")
      val hcl = cr.getOrElse("hcl", "0")
      val ecl = cr.getOrElse("ecl", "0")
      val pid = cr.getOrElse("pid", "0")

      val colors = List("amb", "blu", "brn", "gry", "grn", "hzl", "oth")
      val allowedHcl = List('0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'a', 'b', 'c', 'd', 'e', 'f')

      val validHgt = (
        (hgt.contains("cm") && hgt.dropRight(2).toInt >= 150 && hgt.dropRight(2).toInt <= 193) ||
          (hgt.contains("in") && hgt.dropRight(2).toInt >= 59 && hgt.dropRight(2).toInt <= 76)
        )

      val validHcl = hcl.contains("#") && hcl.drop(1).length == 6 && hcl.drop(1).toCharArray.toList.forall(allowedHcl.contains(_))

      pid.length == 9 &&
        colors.contains(ecl) &&
        (byr >= 1920 && byr <= 2002) &&
        (iyr >= 2010 && iyr <= 2020) &&
        (eyr >= 2020 && eyr <= 2030) &&
        validHcl &&
        validHgt
    })

    println(result.count(_ == true))
  }

  partOne(lines)
  partTwo(lines)
}
