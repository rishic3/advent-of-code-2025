package aoc

import scala.collection.mutable.ArrayBuffer
import scala.io.Source
import scala.math.BigInt

object Day2 {

  // part 1
  def isTwiceRepeated(id: BigInt): Boolean = {
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

  // part 2
  def isRepeated(id: BigInt): Boolean = {
    val s = id.toString
    val len = s.length()

    val candidateSizes = (1 to len / 2).view.filter(sz => len % sz == 0)
    candidateSizes.exists { sz =>
      val iter = s.grouped(sz)
      val substr = iter.next()
      iter.forall(_ == substr)
    }
  }

  def parseLine(line: String): Array[(BigInt, BigInt)] = {
    line.trim()
      .split(',')
      .map(_.split('-'))
      .map(arr => (BigInt(arr(0)), BigInt(arr(1))))
  }

  def computeInvalidIDSum(line: String): BigInt = {
    val ranges = parseLine(line)
    ranges.map { case (low, high) =>
      (low to high).view.filter(isRepeated).sum
    }.sum
  }

  def main(args: Array[String]): Unit = {
    assert(args.length > 0, s"please provide input path")

    val source = Source.fromFile(args(0))
    try {
      println(s"Sum of invalid IDs: ${computeInvalidIDSum(source.mkString)}")
    } finally {
      source.close()
    }
  }
}
