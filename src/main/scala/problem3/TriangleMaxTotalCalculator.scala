package problem3

import scala.annotation.tailrec
import scala.io.Source
import scala.util.{Failure, Success, Try}

object TriangleMaxTotalCalculator extends App {

  val path = "src/main/scala/problem3/resources/input.txt"

  @tailrec
  def maxTotal(input: List[List[Int]], res: Int = 0): Int = {
    if (input.length == 1 || input.isEmpty) {
      res
    } else {
      val updatedInputs = updateInput(input)
      maxTotal(updatedInputs, updatedInputs.last.max)
    }
  }

  inputFromFile.map(elems => maxTotal(elems)) match {
    case Success(result) => println(result)
    case Failure(exception) => println(s"Something went wrong ${exception}")
  }

  private def updateInput(input: List[List[Int]]): List[List[Int]] = {
    val (inputExcludingLastTwoRows, lastTwoRows) = input.partition(x => input.indexOf(x) < input.length - 2)
    val newInput = lastTwoRows.head.zipWithIndex.map {
      case (elem, index) => getMax(lastTwoRows.last, index) + elem
    }
    inputExcludingLastTwoRows :+ newInput
  }

  private def getMax(tail: List[Int], index: Int): Int = {
    if (tail(index) > tail(index + 1)) tail(index) else tail(index + 1)
  }

  private def inputFromFile: Try[List[List[Int]]] = Try {
    Source.fromFile(path).getLines().toList.map(_.split(" ").toList.map(_.toInt))
  }
}
