package io.github.mladensavic94

object Day7 {
  def main(args: Array[String]): Unit = {
    task1()
    task2()
  }

  def task1(): Unit = {
    val list = Util.readFile("src/main/resources/day7.txt")
    val crabs = list.head.split(",").map(_.toInt)
    val max = crabs.max
    val min = crabs.min
    var spentFuel = 1000000000
    var bestPosition = 0
    for (pos <- min to max) {
      val fuel = crabs.map(c => Math.abs(c - pos)).sum
      if (fuel < spentFuel) {
        spentFuel = fuel
        bestPosition = pos
      }
    }
    println(spentFuel)
    println(bestPosition)

  }

  def task2(): Unit = {
    val list = Util.readFile("src/main/resources/day7.txt")
    val crabs = list.head.split(",").map(_.toInt)
    val max = crabs.max
    val min = crabs.min
    var spentFuel = 1000000000
    var bestPosition = 0
    for (pos <- min to max) {
      val fuel = crabs.map(c => factorialIncrease(Math.min(c, pos), Math.max(c, pos))).sum
      if (fuel < spentFuel) {
        spentFuel = fuel
        bestPosition = pos
      }
    }
    println(spentFuel)
    println(bestPosition)
  }

  def factorialIncrease(min: Int, max: Int): Int = {
    var counter = 1
    var sum = 0
    for (i <- min until max) {
      sum += counter
      counter += 1
    }
    sum
  }
}
