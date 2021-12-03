package io.github.mladensavic94

import Day2.{task1, task2}

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
    for (i <- counters.indices) {
      val temp = if (counters(i) >= 500)
        digits.filter(l => l(i) == 1)
      else
        digits.filter(l => l(i) == 0)
      if(temp.length == 1)
        println(temp.head)
    }

  }
}
