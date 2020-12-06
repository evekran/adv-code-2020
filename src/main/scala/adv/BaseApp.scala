package adv

import scala.io.Source

trait BaseApp extends App {

  def filename: String

  def lines: List[String] = {
    val source = Source.fromFile(filename)
    val lines = source.getLines().toList
    source.close()

    lines
  }

  def partOne(): Unit

  def partTwo(): Unit

  partOne()
  partTwo()
}
