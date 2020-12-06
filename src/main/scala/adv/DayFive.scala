package adv

import scala.annotation.tailrec

object DayFive extends BaseApp {
  override def filename: String = "input_day5"

  @tailrec
  def pickSeat(start: Int, end: Int, columns: List[String], directions: (String, String)): Int = {
    val total = end - start
    val half = total / 2

    columns match {
      case x :: xs if x == directions._1 => pickSeat(start, end - half, xs, directions)
      case x :: xs if x == directions._2 => pickSeat(end - half, end, xs, directions)
      case Nil => start
    }
  }

  def getSeatId(code: List[String]) = {
    val (rows, columns) = code.splitAt(7)

    val row = pickSeat(0, 128, rows, ("F", "B"))
    val column = pickSeat(0, 7, columns, ("L", "R"))

    row * 8 + column
  }

  def partOne(): Unit = {
    val maxId = lines.map(x => getSeatId(x.split("").toList)).max

    println(maxId)
  }

  def partTwo(): Unit = {
    val r = lines.map(x => getSeatId(x.split("").toList)).sorted

    @tailrec
    def findMySeat(seatIds: List[Int]): Int = {
      seatIds match {
        case a :: xs if xs.head - a == 1 => findMySeat(xs)
        case a :: b :: _ if b - a != 1 => a + 1
        case _ => 0
      }
    }

    val seat = findMySeat(r)

    println(seat)
  }
}
