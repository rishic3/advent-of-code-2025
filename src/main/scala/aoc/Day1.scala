package aoc

import scala.io.Source

class Dial(dialStart: Int, dialSize: Int) {
  var currPos = dialStart

  // part 1
  def adjust(distance: Int, clockwise: Boolean): Unit = {
    currPos = if (clockwise) currPos + distance else currPos - distance
    currPos = Math.floorMod(currPos, dialSize)
  }

  // part 2
  var zeroClicks = 0
  def adjustClicks(distance: Int, clockwise: Boolean): Unit = {
    val includePass = currPos != 0
    val modDist = distance % dialSize
    zeroClicks += distance / dialSize

    if (clockwise) {
      currPos += modDist
      if (includePass && currPos >= dialSize) zeroClicks += 1
    } else {
      currPos -= modDist
      if (includePass && currPos <= 0) zeroClicks += 1
    }

    currPos = Math.floorMod(currPos, dialSize)
  }
}

object Day1 {

  // part 1
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

  // part 2
  def computeClickPassword(source: scala.io.BufferedSource): Int = {
    val dial = new Dial(50, 100)

    for (line <- source.getLines()) {
      line match {
        case s if s.startsWith("L") => dial.adjustClicks(s.tail.toInt, false)
        case s if s.startsWith("R") => dial.adjustClicks(s.tail.toInt, true)
        case _ => throw new IllegalArgumentException(s"illegal input: $line")
      }
      println(s"input: $line, zeroClicks: ${dial.zeroClicks}, currPos: ${dial.currPos}")
    }

    dial.zeroClicks
  }

  def main(args: Array[String]): Unit = {
    assert(args.length > 0, s"please provide input path")

    val source = Source.fromFile(args(0))
    try {
      // println(s"Password: ${computePassword(source)}")
      println(s"Password: ${computeClickPassword(source)}")
    } finally {
      source.close()
    }
  }
}
