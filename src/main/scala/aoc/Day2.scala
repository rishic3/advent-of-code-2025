package aoc

import scala.collection.mutable.ArrayBuffer
import scala.io.Source
import scala.math.BigInt

object Day2 {

  // part 1
  def isInvalid1(id: BigInt): Boolean = {
    val idStr = id.toString
    val len = idStr.length()
    if (len % 2 == 0) {
      val p1 = idStr.substring(0, len / 2)
      val p2 = idStr.substring(len / 2, len)
      p1 == p2
    } else {
      false
    }
  }

  def parseLine(line: String): Array[(BigInt, BigInt)] = {
    line.trim().split(',').map(_.split('-')).map(arr => (BigInt(arr(0)), BigInt(arr(1))))
  }

  def computeInvalidIDSum(line: String): BigInt = {
    val ranges = parseLine(line)
    ranges.foldLeft(BigInt(0)) { case (acc, (low, high)) =>
      val range = low to high
      acc + range.foldLeft(BigInt(0)) { case (acc, id) =>
        if (isInvalid1(id)) acc + id else acc
      }
    }
  }

  def main(args: Array[String]): Unit = {
    assert(args.length > 0, s"please provide input path")

    val source = Source.fromFile(args(0)).mkString
    println(s"Sum of invalid IDs: ${computeInvalidIDSum(source)}")
  }
}
