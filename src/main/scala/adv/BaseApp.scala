package adv

import scala.io.Source

trait BaseApp extends App {

  def filename: String

  def lines: List[String] = Source.fromFile(filename).getLines.toList
}
