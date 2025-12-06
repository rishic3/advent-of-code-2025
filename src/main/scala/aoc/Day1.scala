package aoc

import scala.io.Source

class Dial(dialStart: Int, dialSize: Int) {
  var currPos = dialStart

  def adjust(distance: Int, clockwise: Boolean): Unit = {
    currPos = if (clockwise) currPos + distance else currPos - distance
    currPos = Math.floorMod(currPos, dialSize)
  }
}

object Day1 {

  def computePassword(source: scala.io.BufferedSource): Int = {
    val dial = new Dial(50, 100)
    var zeroCount = 0

    for (line <- source.getLines()) {
      line match {
        case s if s.startsWith("L") => dial.adjust(s.tail.toInt, false)
        case s if s.startsWith("R") => dial.adjust(s.tail.toInt, true)
        case _ => throw new IllegalArgumentException(s"illegal input: $line")
      }
      if (dial.currPos == 0) zeroCount += 1
    }

    zeroCount
  }

  def main(args: Array[String]): Unit = {
    assert(args.length > 0, s"please provide input path")

    val source = Source.fromFile(args(0))
    try {
      println(s"Password: ${computePassword(source)}")
    } finally {
      source.close()
    }
  }
}
