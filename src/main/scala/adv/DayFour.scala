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

  def extractCredentialsFromInput(input: List[String]): List[String] = {
    val credentialsSeparator = "@"
    input.map {
      case inputRow if inputRow == "" => credentialsSeparator
      case x => x
    }.mkString(" ").split(credentialsSeparator).toList
  }

  def credentialsToMap(credentials: String): Map[String, String] = {
    val attributeSeparator = ":"
    credentials.split(" ")
      .toList
      .filter(_.nonEmpty)
      .map { attribute =>
        attribute.split(attributeSeparator).toList match {
          case key :: value :: Nil => key -> value
        }
      }.toMap
  }

  def getCredentialsWithRequiredFields(input: List[String]): List[Map[String, String]] = {
    val credentialsMap = extractCredentialsFromInput(input).map(credentialsToMap)

    credentialsMap.filter { credentials =>
      val credentialsAttributes = credentials.keys.toList
      requiredFields.forall(field => credentialsAttributes.contains(field))
    }
  }

  def partOne(input: List[String]): Unit = {
    val result = getCredentialsWithRequiredFields(input)

    println(result.length)
  }

  def partTwo(input: List[String]): Unit = {
    val credentials = getCredentialsWithRequiredFields(input)

    val result = credentials.map(credentialsMap => {
      val byr = credentialsMap.getOrElse("byr", "0").toInt
      val iyr = credentialsMap.getOrElse("iyr", "0").toInt
      val eyr = credentialsMap.getOrElse("eyr", "0").toInt

      val hcl = credentialsMap.getOrElse("hcl", "")
      val ecl = credentialsMap.getOrElse("ecl", "")

      val hgt = credentialsMap.getOrElse("hgt", "")

      val pid = credentialsMap.getOrElse("pid", "")

      val allowedEcl = List("amb", "blu", "brn", "gry", "grn", "hzl", "oth")
      val validEcl = allowedEcl.contains(ecl)

      val hgtValue = hgt.dropRight(2).toInt
      val validHgt =
        (hgt.contains("cm") && hgtValue >= 150 && hgtValue <= 193) ||
          (hgt.contains("in") && hgtValue >= 59 && hgtValue <= 76)

      val allowedHcl = List('0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'a', 'b', 'c', 'd', 'e', 'f')
      val hclValue = hcl.drop(1).toCharArray.toList
      val validHcl = hcl.contains("#") && hclValue.length == 6 && hclValue.forall(allowedHcl.contains)

      pid.length == 9 &&
        (byr >= 1920 && byr <= 2002) &&
        (iyr >= 2010 && iyr <= 2020) &&
        (eyr >= 2020 && eyr <= 2030) &&
        validEcl &&
        validHcl &&
        validHgt
    })

    println(result.count(_ == true))
  }

  partOne(lines)
  partTwo(lines)
}
