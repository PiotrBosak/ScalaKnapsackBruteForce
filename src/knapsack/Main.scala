package knapsack

import java.io.File
import java.util.Scanner

object Main extends App {
  val weightLineNumber = 0
  val costLineNumber = 1
  val maxWeightLineNumber = 2
  val path = "src/resources/knapsack.txt"

  class TupleWrapper(val t: (Int, Int))

  val weightLine = getLine(path, weightLineNumber)
  val weights = getNumbers(weightLine)
  val costLine = getLine(path, costLineNumber)
  val costs = getNumbers(costLine)
  val maxWeightLine = getLine(path, maxWeightLineNumber)
  val maxWeight = getNumber(maxWeightLine)

  val zippedList = weights.zip(costs)
  val set = zippedList.map(new TupleWrapper(_)).toSet
  val subsets = set.subsets()
  val filtered = subsets.filter(calculateWeight(_) <= maxWeight)
  val max = filtered.maxBy(calculateCost)

  println(max.size)
  val mapped = max.map(x => x.t)
  mapped.foreach(println)

  def calculateWeight(set: Set[TupleWrapper]): Int = {
    if (set.isEmpty) 0
    else set
      .map(x => x.t)
      .toList
      .map(_._1)
      .sum
  }

  def calculateCost(set: Set[TupleWrapper]): Int = {
    if (set.isEmpty) 0
    else set
      .map(x => x.t)
      .toList
      .map(_._2)
      .sum
  }

  def getNumbers(line: String) = {
    line.split("\\D+")
      .filter(_.nonEmpty)
      .map(x => x.toInt)
      .toList
  }

  def getNumber(line: String) = getNumbers(line).head

  def getLine(path: String, lineNumber: Int): String = {
    val file = new File(path)
    val scanner = new Scanner(file)

    @scala.annotation.tailrec
    def aux(acc: Int = 0): String =
      if (acc >= lineNumber) scanner.nextLine()
      else {
        scanner.nextLine()
        aux(acc + 1)
      }

    aux()
  }


}
