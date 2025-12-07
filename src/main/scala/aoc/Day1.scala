package aoc

import scala.io.Source

case class Dial(pos: Int, size: Int, zeroClicks: Int = 0) {

  // part 1
  def adjust(distance: Int, clockwise: Boolean): Dial = {
    val dir = if (clockwise) 1 else -1
    val newPos = Math.floorMod(pos + (distance * dir), size)
    copy(pos = newPos)
  }

  // part 2
  def adjustClicks(distance: Int, clockwise: Boolean): Dial = {
    val modDist = distance % size
    val revolutions = distance / size

    val (newPos, crossedZero) = if (clockwise) {
      val next = pos + modDist
      val crossed = pos != 0 && next >= size
      (next, crossed)
    } else {
      val next = pos - modDist
      val crossed = pos != 0 && next <= 0
      (next, crossed)
    }

    val addedClicks = revolutions + (if (crossedZero) 1 else 0)

    copy(pos = Math.floorMod(newPos, size), zeroClicks = zeroClicks + addedClicks)
  }
}

object Day1 {

  sealed trait Action
  case class Left(distance: Int) extends Action
  case class Right(distance: Int) extends Action

  def parseAction(line: String): Action = line.head match {
    case 'L' => Left(line.tail.toInt)
    case 'R' => Right(line.tail.toInt)
    case _ => throw new IllegalArgumentException(s"illegal input: $line")
  }

  // part 1
  def computePassword(lines: Iterator[String]): Int = {
    val initDial = new Dial(pos = 50, size = 100)

    val finalDial = lines.foldLeft((initDial, 0)) { case ((dial, zeroClicks), line) =>
      val action = parseAction(line)
      val newDial = action match {
        case Left(dist) => dial.adjust(dist, clockwise = false)
        case Right(dist) => dial.adjust(dist, clockwise = true)
      }
      val newClicks = if (newDial.pos == 0) zeroClicks + 1 else zeroClicks
      (newDial, newClicks)
    }

    finalDial._2
  }

  // part 2
  def computeClickPassword(lines: Iterator[String]): Int = {
    val initDial = new Dial(pos = 50, size = 100)

    val finalDial = lines.foldLeft(initDial) { case (dial, line) =>
      parseAction(line) match {
        case Left(dist) => dial.adjustClicks(dist, clockwise = false)
        case Right(dist) => dial.adjustClicks(dist, clockwise = true)
      }
    }

    finalDial.zeroClicks
  }

  def main(args: Array[String]): Unit = {
    assert(args.length > 0, s"please provide input path")

    val source = Source.fromFile(args(0))
    try {
      val lines = source.getLines()

      // println(s"Password: ${computePassword(lines)}")
      println(s"Password: ${computeClickPassword(lines)}")
    } finally {
      source.close()
    }
  }
}
