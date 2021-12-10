package io.github.mladensavic94
package completed

object Day3 {
  def main(args: Array[String]): Unit = {
    task1()
    task2()
  }

  def task1(): Unit = {
    val list = Util.readFile("src/main/resources/day3.txt")
    val sums = list.map(s => s.toList.map(l => l.asDigit))
    var counters = List.fill(12)(0)
    for (sum <- sums) {
      counters = (counters, sum).zipped.map(_ + _)
    }
    val mcb = counters.map(i => if (i > 500) "1" else "0").mkString("")
    val lcb = counters.map(i => if (i > 500) "0" else "1").mkString("")
    println(Integer.parseInt(mcb, 2) * Integer.parseInt(lcb, 2))

  }

  def task2(): Unit = {
    val list = Util.readFile("src/main/resources/day3.txt")
    val digits = list.map(s => s.toList.map(l => l.asDigit))
    var counters = List.fill(12)(0)
    for (sum <- digits) {
      counters = (counters, sum).zipped.map(_ + _)
    }
    println(counters)
    var mostCommonBits = List.from(digits)
    var counter = 0
    while (mostCommonBits.length > 1) {
      mostCommonBits = if (counters(counter) >= mostCommonBits.length / 2.0)
        mostCommonBits.filter(l => l(counter) == 1)
      else
        mostCommonBits.filter(l => l(counter) == 0)
      counter += 1
      counters = List.fill(12)(0)
      for (sum <- mostCommonBits) {
        counters = (counters, sum).zipped.map(_ + _)
      }
      println(mostCommonBits)
      println(counters)
    }
    var counters2 = List.fill(12)(0)
    for (sum <- digits) {
      counters2 = (counters2, sum).zipped.map(_ + _)
    }
    var leastCommonBits = List.from(digits)
    var counter2 = 0
    while (leastCommonBits.length > 1) {
      leastCommonBits = if (counters2(counter2) >= leastCommonBits.length / 2.0)
        leastCommonBits.filter(l => l(counter2) == 0)
      else
        leastCommonBits.filter(l => l(counter2) == 1)
      counter2 += 1
      counters2 = List.fill(12)(0)
      for (sum <- leastCommonBits) {
        counters2 = (counters2, sum).zipped.map(_ + _)
      }
    }
    println(Integer.parseInt(leastCommonBits.head.mkString(""), 2) * Integer.parseInt(mostCommonBits.head.mkString(""), 2))

  }
}
